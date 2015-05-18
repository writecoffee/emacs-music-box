(require 'json)
(require 'helm)

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
                                        "limit=20"
                                        "&s="
                                        MACRO-SONG-QUERY
                                        "&type=1"
                                        "&sub=false"
                                        "&offset=0"
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
  (let ((formatted-query-words search-query))
    (json-read-from-string
     (shell-command-to-string
      (replace-regexp-in-string MACRO-SONG-QUERY
                                formatted-query-words
                                CMD-CURL-SEARCH-SONGS)))))

(defun helm-music-163-search (search-query)
  (let* ((lambda-format-for-display (lambda (song)
                                      (cdr (assoc 'name song))))
         (lambda-get-song-download-url (lambda (song)
                                         (let ((song-detail (cmd-get-song-detail
                                                             (number-to-string (cdr (assoc 'id song))))))
                                           (cdr (assoc 'mp3Url
                                                       (elt (cdr (assoc 'songs song-detail))
                                                            0))))))
         (json-response (cmd-search-songs search-query))
         (song-list (cdr (assoc 'songs (assoc 'result json-response)))))

    (mapcar (lambda (song)
              (cons (funcall lambda-format-for-display song)
                    (funcall lambda-get-song-download-url song)))
            song-list)))

(defvar musics (helm-music-163-search "lady+gaga"))

(defun helm-music-163-play-song (song-url)
  (async-shell-command (concat "mpg123 "
                               song-url)
                       MUSIC-PLAYER-BUFFER-NAME))

(defvar helm-sources-music-163
  '((name . "NetEase Music")
    (candidates . musics)
    (action . (("Play track" . helm-music-163-play-song)))))

(helm :sources '(helm-sources-music-163))
