;;; helm-netease-music.el --- Listen to NetEase music in Emacs.
;; Copyright 2013 Silao Xu
;;
;; Author: Silao Xu <writecoffee@gmail.com>
;; Maintainer: Silao Xu <writecoffee@gmail.com>
;; URL: https://github.com/writecoffee/emacs-music-box
;; Created: July 4th  2015
;; Version: 0.1.1
;; Package-Requires: ((helm "0.0.0") (multi "2.0.0"))

;;; Commentary:
;;
;; A search & play interface for NetEase FREE Music.
;; Fully inspired by helm-spotify created by Kris Jenkins <krisajenkins@gmail.com>
;;
;; Currently supports OSX

;;; Code:

(require 'json)
(require 'helm)
(require 'multi)

(defun encrypt-id (id)
  (lexical-let* ((magic (vconcat "3go8&$8*3*3h0k(2)2"))
                 (song-bytes (vconcat id))
                 (magic-len (length magic))
                 (magic-song-bytes (reduce (lambda (magic-id current-byte)
                                             (lexical-let ((i (length magic-id)))
                                               (append magic-id
                                                       (list (logxor (elt magic (mod i magic-len))
                                                                     current-byte)))))
                                           song-bytes
                                           :initial-value nil))
                 (replaced-slash (mapconcat 'identity
                                            (split-string (util::base64-encode-hex-string
                                                           (md5 (concat magic-song-bytes)))
                                                          "/")
                                            "_"))
                 (replaced-plus (mapconcat 'identity
                                           (split-string (util::base64-encode-hex-string
                                                          (md5 (concat magic-song-bytes)))
                                                         "+")
                                           "-")))
    replaced-plus))

(defconst CMD-CURL-SEARCH-SONGS (concat "curl -s "
                                        "--Cookie 'appver=2.0.2' "
                                        "--Referer 'http://music.163.com' "
                                        "-X POST "
                                        "\""
                                        "http://music.163.com/api/search/get/?"
                                        "limit=20"
                                        "&type=1"
                                        "&sub=false"
                                        "&offset=0"
                                        "&s=%s"
                                        "\""))

(defconst CMD-CURL-GET-SONG-DETAIL (concat "curl -s "
                                           "--Cookie 'appver=2.0.2' "
                                           "--Referer 'http://music.163.com' "
                                           "\""
                                           "http://music.163.com/api/song/detail/?"
                                           "id="
                                           "%s"
                                           "&ids=%5B"
                                           "%s"
                                           "%5D"
                                           "\""))

(defconst MUSIC-PLAYER-BUFFER-NAME "** mpg123 music player **")

(defun cmd-get-song-detail (id)
  (json-read-from-string 
   (shell-command-to-string
    (concat "curl -s "
            "--Cookie 'appver=2.0.2' "
            "--Referer 'http://music.163.com' "
            "\""
            "http://music.163.com/api/song/detail/?"
            "id="
            id
            "8&ids=%5B"
            id
            "%5D"
            "\""))))

(defun cmd-search-songs (search-query)
  (let ((formatted-query-words (url-encode-url search-query)))
    (json-read-from-string
     (shell-command-to-string
      (format CMD-CURL-SEARCH-SONGS formatted-query-words)))))

(defun helm-netease-music-search ()
  (let* ((lambda-format-for-display (lambda (song)
                                      (let ((name (cdr (assoc 'name song)))
                                            (album-name (cdr (assoc 'name (cdr (assoc 'album song)))))
                                            (artist-names (mapcar (lambda (artist)
                                                                    (cdr (assoc 'name artist)))
                                                                  (cdr (assoc 'artists song))))
                                            (track-length (cdr (assoc 'duration song))))
                                        (format "%s (%d min %0.2d sec)\n%s - %s"
                                                name
                                                (/ track-length 60000) (/ (mod track-length 60000) 1000)
                                                (mapconcat 'identity artist-names "/")
                                                album-name))))

         (json-response (cmd-search-songs helm-pattern))
         (song-list (cdr (assoc 'songs (assoc 'result json-response))))
         (result-list
          (mapcar (lambda (song)
                    (cons (funcall lambda-format-for-display song)
                          (number-to-string (cdr (assoc 'id song)))))
                  song-list)))
    result-list))

(setq kill-buffer-query-functions
  (remq 'process-kill-buffer-query-function
         kill-buffer-query-functions))

(defun helm-netease-music-play-song (song-url)
  (let ((lambda-get-song-download-url
          (lambda ()
            (let ((song-detail (cmd-get-song-detail song-url)))
              (cdr (assoc 'mp3Url
                          (elt (cdr (assoc 'songs song-detail))
                               0)))))))
    (set-buffer-modified-p nil)
    (when (get-buffer MUSIC-PLAYER-BUFFER-NAME)
      (kill-buffer MUSIC-PLAYER-BUFFER-NAME))
    (async-shell-command (concat "mpg123 " (funcall lambda-get-song-download-url))
                         MUSIC-PLAYER-BUFFER-NAME)
    (delete-windows-on MUSIC-PLAYER-BUFFER-NAME)))

(defun helm-netease-actions (actions track)
  "Return a list of helm ACTIONS available for this TRACK."
  `((,(format "Play Track - %s" (alist-get '(name) track))       . spotify-play-track)
    (,(format "Play Album - %s" (alist-get '(album name) track)) . spotify-play-album)
    ("Show Track Metadata" . pp)))

(defvar helm-sources-netease-music
  '((name . "NetEase Music")
    (volatile)
    (delayed)
    (multiline)
    (requires-pattern . 2)
    (candidates-process . helm-netease-music-search)
    (action . (("Play track" . helm-netease-music-play-song)))))

(defun helm-netease-music ()
  "Netease music search interface in helm."
  (interactive)
  (let ((back-helm-input-idle-delay helm-input-idle-delay))
    (setq helm-input-idle-delay 1)
    (helm :sources '(helm-sources-netease-music)
          :buffer "*helm-netease-music*")
    (setq helm-input-idle-delay back-helm-input-idle-delay)))

(provide 'helm-netease-music)
;;; helm-netease-music.el ends here
