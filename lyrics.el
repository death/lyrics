;;; lyrics.el --- Get lyrics for currently-playing song

;; Author: DEATH
;; Created: 2009

;;; Commentary:

;; Get lyrics for currently-playing song.

;;; Code:

(require 'url)
(require 'url-http)
(require 'libmpdee)

(defun lyrics-current-song ()
  "Return a list, whose elements are the artist and title of the
currently-playing song.  If for some reason such a list cannot be
constructed, signal an error."
  (let* ((song (mpd-get-current-song mpd-inter-conn))
         (artist (getf song 'Artist))
         (title (getf song 'Title)))
    (if (and artist title)
        (list artist title)
      (error "Can't get currently-playing song information"))))

(defun lyrics-get-song-url (format artist title)
  "Return a URL that designates the song's lyrics in the given
format, if found."
  (concat "http://lyricwiki.org/api.php?func=getSong"
          "&artist=" (lyrics-encode-argument artist)
          "&song=" (lyrics-encode-argument title)
          "&fmt=" format))

(defun lyrics-encode-argument (string)
  "Return lyricwiki-friendly argument string."
  (url-hexify-string (replace-regexp-in-string " " "_" string)))

(defun lyrics-fetch (artist title)
  "Fetch lyrics for song."
  (url-retrieve (lyrics-get-song-url "text" artist title)
                'lyrics-fetch-callback
                (list artist title)))

(defun lyrics-fetch-callback (status artist title)
  "Called when fetch information is available."
  (cond ((assq :error status)
         (let ((info (cdr (assq :error status))))
           (signal (car info) (cdr info))))
        (t (let ((code (url-http-parse-response)))
             (case code
               (200 (let ((response-buffer (current-buffer)))
                      (re-search-forward "^$")
                      (forward-char)
                      (unwind-protect
                          (if (looking-at "^Not found$")
                              (error "No lyrics for this song")
                            (lyrics-view response-buffer (point) artist title))
                        (url-mark-buffer-as-dead response-buffer))))
               (t (url-mark-buffer-as-dead (current-buffer))
                  (error "Couldn't fetch lyrics, server returned code %d" code)))))))

(defun lyrics-buffer-name (artist title)
  "Return lyrics buffer name for song."
  (concat "*Lyrics " artist " - " title "*"))

(defun lyrics-view (response-buffer start artist title)
  "View the lyrics in a dedicated buffer."
  (with-current-buffer (get-buffer-create (lyrics-buffer-name artist title))
    (setq buffer-read-only nil)
    (erase-buffer)
    (insert-buffer-substring response-buffer start)
    (goto-char (point-min))
    (setq buffer-read-only t)
    (view-buffer-other-window (current-buffer) nil 'kill-buffer)))

(defun lyrics (&optional prompt)
  "Try to fetch and view lyrics for currently-playing song,
except when a prefix argument is supplied, in which case the song
is prompted for."
  (interactive "P")
  (condition-case err
      (apply #'lyrics-fetch
             (if prompt
                 (list (read-string "Artist: ")
                       (read-string "Title: "))
               (lyrics-current-song)))
    (error (message "Lyrics error: %s" (error-message-string err)))))

(provide 'lyrics)
