(defun record-global-position-for-back-button (&rest not-used-args)
  "Push mark, and add it to `global-mark-ring'.

This function differs from `push-mark' in that `global-mark-ring'
is always updated."
  (let (location (point-marker))
    (callf or location (point))
    (push-mark location)
    ;; (when (and (eq consecutives 'limit)
    ;;            (eq (marker-buffer (car global-mark-ring)) (current-buffer)))
    ;;   (move-marker (car global-mark-ring) nil)
    ;;   (pop global-mark-ring))
    (push (copy-marker (mark-marker)) global-mark-ring)
    (when (> (length global-mark-ring) global-mark-ring-max)
      (move-marker (car (nthcdr global-mark-ring-max global-mark-ring)) nil)
      (setcdr (nthcdr (1- global-mark-ring-max) global-mark-ring) nil)
      )
    )
  )


(defun pop-ring-while-dupe-or-minibuff (el)
  (if (or (minibufferp (marker-buffer (car global-mark-ring)))
          (and el (equal el (car global-mark-ring))))
      (progn
        (pop global-mark-ring)
        (pop-ring-while-dupe-or-minibuff el)
        )
    )
  )

(defun press-back-button ()
  (interactive)
  (pop-ring-while-dupe-or-minibuff (point-marker)) ;; If same as current marker.
  (let ((old-mark (car global-mark-ring)))
    (ring-insert-at-beginning forward-button-ring old-mark)
    (pop-global-mark)
    (pop-ring-while-dupe-or-minibuff old-mark)
    )
  )

(defun press-forward-button ()
  (interactive)
  (let ((forward-mark (ring-remove forward-button-ring)))
    (progn
      (goto-char forward-mark)
      (record-global-position-for-back-button)
      )
    )
  )

(setq forward-button-ring (make-ring 2000))
(setq global-mark-ring-max 2000)

(defun forward-button-or-local-ring (arg)
  (interactive "P")
  (if arg
      (helm-mark-ring)
    (press-forward-button))
  )

(defun back-button-or-global-ring (arg)
  (interactive "P")
  (if arg
      (helm-global-mark-ring)
    (press-back-button))
  )

(global-unset-key (kbd "<C-M-left>"))
(global-set-key (kbd "<C-M-left>") 'back-button-or-global-ring)
(global-unset-key (kbd "<C-M-right>"))
(global-set-key (kbd "<C-M-right>") 'forward-button-or-local-ring)

<<<<<<< HEAD
;; TODO: Find a way to avoid all this copypasta. I.e. supply "list of functions to advice on" and iter it later.
=======
;; TODO: Find a way to avoid all this copypasta.
>>>>>>> ecff495e20b8b1f6f1ffa08b2ade8420e7f0ac05

;; Record position before evil window movement
(advice-add 'evil-window-right :before #'record-global-position-for-back-button)
(advice-add 'evil-window-left :before #'record-global-position-for-back-button)
(advice-add 'evil-window-down :before #'record-global-position-for-back-button)
(advice-add 'evil-window-up :before #'record-global-position-for-back-button)
(advice-add 'evil-goto-line :before #'record-global-position-for-back-button)

;; Record position on vim enter and exit the insert state
(add-hook 'evil-insert-state-entry-hook #'record-global-position-for-back-button)
(add-hook 'evil-insert-state-exit-hook #'record-global-position-for-back-button)
(add-hook 'evil-jumps-pre-jump-hook #'record-global-position-for-back-button)

;; Record position before tag navigation
(advice-add 'senator-previous-tag :before #'record-global-position-for-back-button)
(advice-add 'senator-next-tag :before #'record-global-position-for-back-button)

;; Record position before helm functions
<<<<<<< HEAD
;; Many functions, like "Go to tag" pick the position based on it.
=======
>>>>>>> ecff495e20b8b1f6f1ffa08b2ade8420e7f0ac05
(advice-add 'helm-buffers-list :before #'record-global-position-for-back-button)
(advice-add 'helm-buffers-list :before #'record-global-position-for-back-button)
(advice-add 'helm-confirm-and-exit-minibuffer :before #'record-global-position-for-back-button)
(advice-add 'helm-maybe-exit-minibuffer :before #'record-global-position-for-back-button)
(advice-add 'helm-execute-persistent-action :before #'record-global-position-for-back-button)

;; Org mode
(advice-add 'org-goto :before #'record-global-position-for-back-button)
(advice-add 'org-open-at-point :before #'record-global-position-for-back-button)

;; Bookmark
(advice-add 'bookmark-jump :before #'record-global-position-for-back-button)
<<<<<<< HEAD

;; Ag/Dumb jump
(advice-add 'helm-projectile-ag :before #'record-global-position-for-back-button)
(advice-add 'dumb-jump-go :before #'record-global-position-for-back-button)
=======
>>>>>>> ecff495e20b8b1f6f1ffa08b2ade8420e7f0ac05
