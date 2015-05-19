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

(defconst MACRO-SONG-QUERY "${QUERY_WORDS}")
(defconst MACRO-SONG-ID "${SONG_ID}")

(defconst CMD-CURL-SEARCH-SONGS (concat "curl -s "
                                        "--Cookie 'appver=2.0.2' "
                                        "--Referer 'http://music.163.com' "
                                        "-X POST "
                                        "\""
                                        "http://music.163.com/api/search/get/?"
                                        "limit=2"
                                        "&type=1"
                                        "&sub=false"
                                        "&offset=0"
                                        "&s="
                                        MACRO-SONG-QUERY
                                        "\""))

(defconst CMD-CURL-GET-SONG-DETAIL (concat "curl -s "
                                           "--Cookie 'appver=2.0.2' "
                                           "--Referer 'http://music.163.com' "
                                           "\""
                                           "http://music.163.com/api/song/detail/?"
                                           "id="
                                           MACRO-SONG-ID
                                           "8&ids=%5B"
                                           MACRO-SONG-ID
                                           "%5D"
                                           "\""))

(defconst MUSIC-PLAYER-BUFFER-NAME "** mpg123 music player **")

(defun cmd-get-song-detail (id)
  (json-read-from-string 
   (shell-command-to-string
    (replace-regexp-in-string MACRO-SONG-ID
                              id
                              CMD-CURL-GET-SONG-DETAIL))))

(defun cmd-search-songs (search-query)
  (let ((formatted-query-words (url-hexify-string search-query)))
    (json-read-from-string
     (shell-command-to-string
      (replace-regexp-in-string MACRO-SONG-QUERY
                                formatted-query-words
                                CMD-CURL-SEARCH-SONGS)))))

(defun helm-netease-music-search ()
  (let* ((lambda-format-for-display (lambda (song)
                                      (cdr (assoc 'name song))))
         (json-response (cmd-search-songs helm-pattern))
         (song-list (cdr (assoc 'songs (assoc 'result json-response))))
         (result-list
          (mapcar (lambda (song)
                    (cons (funcall lambda-format-for-display song)
                          (number-to-string (cdr (assoc 'id song)))))
                  song-list)))
    result-list))

(defun helm-netease-music-play-song (song-url)
  (let ((lambda-get-song-download-url
          (lambda ()
            (let ((song-detail (cmd-get-song-detail song-url)))
              (cdr (assoc 'mp3Url
                          (elt (cdr (assoc 'songs song-detail))
                               0)))))))

    (async-shell-command
     (concat "mpg123 "
             (funcall lambda-get-song-download-url))
     MUSIC-PLAYER-BUFFER-NAME)))

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

(helm-netease-music)
