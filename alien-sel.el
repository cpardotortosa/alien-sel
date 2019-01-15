;;; -*- coding: utf-8 -*-
;;; alien-sel.el --- A library for user selection/completion.

;; Copyright (C) 2019 Cecilio Pardo.

;; Author: Cecilio Pardo <cpardo@imayhem.com>
;; Keywords: selection, completion

(require 'dash)
(require 's)
(require 'subr-x)
(require 'cl)

(defgroup alien-sel-faces nil
  "Faces for alien-sel."
  :group 'faces)

(defface alien-sel-logo-face '((t :foreground "red"))
  "The face for the alien logo /A\\ that appears before prompts."
  :group 'alien-sel-faces)

(defface alien-sel-light-face '((t :foreground "#808080"))
  "Use for annotating the selection list, should be 'light' to
not get in the way too much"
  :group 'alien-sel-faces )

(defvar alien-sel-visible-item-max-count-before-selected 5
  "Maximum number of items shown in the listing before the
  current selection. The listing is scrolled to enforce this.")

(defvar alien-sel-visible-item-max-count 12
  "Maximum number of items shown in the listing.")

(defvar alien-sel-show-line-numbers t
  "Show line numbers to the left of items.")

(defvar alien-sel-filter-type 'prefix-substring-flex
  "Initial filter type. The use can switch types on the fly.")

(defun -alien-sel-prompt(prompt &optional nocolon)
  "Builds the prompt from a string, adding the Alien logo"
  (format "%s %s%s"
          (propertize "/A\\" 'face 'alien-sel-logo-face )
          prompt
          (if nocolon "" ": ")))

(defun -alien-sel-header-line()
  "Sets the header line for the listing buffer. It includes the
prompt, and the number of items (passing filter/total)."
  (let* ((l1 (-alien-sel-prompt -alien-sel-prompt t))
         (l2 (propertize (format
                          " %d/%d items"
                          (length -alien-sel-options-filtered)                          
                          (length -alien-sel-options)))))
    (setq header-line-format (format " %s       %s" l1 l2))))

(defun -alien-sel-propertize-for-filter(option filter)
  "Propertize a string that matches the filter, to show how it
does. Underline the matching characters."
  (if (eq alien-sel-filter-type 'regexp)
      (progn
        (string-match filter option)
        (add-face-text-property (match-beginning 0) (match-end 0) 'underline nil option))
    (cond
     ;; Substring matching
     ((string-match (regexp-quote filter) option)
      (add-face-text-property (match-beginning 0) (match-end 0) 'underline nil option))
     
     ((let ((option-as-list (append option nil))
            (filter-as-list (append filter nil))
            (index-into-filter 0)
            (case-fold-search t))
        
        (--each-indexed option-as-list
          ;; If this char is filter[index-into-filter]:
          ;;  - Underline this char.
          ;;  - Increment index-into-filter
          (let ((filter-char (nth index-into-filter filter-as-list)))
            (when filter-char
              (when (char-equal filter-char it)
                (add-face-text-property it-index (1+ it-index) 'underline nil option)
                (incf index-into-filter))))))))))

(defun -alien-sel-flex-regexp(filter)
  "Builds the regexp used for flex matching"
  (if (string-empty-p filter)
      ""
    (let* ((aslist (append filter nil))
           (result (regexp-quote (string (car aslist)))))
      (dolist (char (cdr aslist))
        (setq result (concat result (string ?. ?* )(regexp-quote (char-to-string char)))))
      result)))

(defun -alien-sel-apply-filter()
  "Apply the filter. Sets `-alien-sel-options-filtered' with the passing items."
  (let* ((re (regexp-quote -alien-sel-filter))
         (regexp-1 (concat "^" re))
         (regexp-2 re)
         (regexp-3 (-alien-sel-flex-regexp -alien-sel-filter))
         (scorer

          (cond
           ((eq alien-sel-filter-type 'prefix-substring-flex)
            (lambda(option filter)
              (cond
               ;; If the string starts with the prefix filter, gets a score of 3. This includes a perfect match.
               ((string-match-p regexp-1 option) 3)   
               ;; If the string contains the entire substring gets a score of 2
               ((string-match-p regexp-2 option) 2)
               ;; "flex match" get 1 point 
               ((string-match-p regexp-3 option) 1))))
                      
           ((eq alien-sel-filter-type 'prefix-substring)
            (lambda(option filter)
              (cond
               ;; If the string starts with the prefix filter, gets a score of 3. This includes a perfect match.
               ((string-match-p regexp-1 option) 3)   
               ;; If the string contains the entire substring gets a score of 2
               ((string-match-p regexp-2 option) 2))))
           
           ((eq alien-sel-filter-type 'prefix)
            (lambda(option filter)
              (cond
               ((string-match-p regexp-1 option) 3))))

           ((eq alien-sel-filter-type 'regexp)
            (lambda(option filter)
              (condition-case nil
                  (if (string-match-p -alien-sel-filter option) 1)
                (error
                 ;; TODO: Signal the user that the regexp is incorrect. In a nonintrusive way, as the regexp may be just
                 ;; still incomplete.
                 nil)))))))

    (setq -alien-sel-options-filtered
          (--sort
           (> (get-text-property 0 'alien-sel-score it)
              (get-text-property 0 'alien-sel-score other))
           (--keep
            (let ((score (apply scorer (list it -alien-sel-filter))))
              (if score
                  ;;(format "%s (%d)"
                  (propertize it 'alien-sel-score score)
                ;; score)
                nil))
            -alien-sel-options)))))

(defun -alien-sel-setup-buffer(prompt options)
  "After creating the selection buffer, sets the global
variables, displays the buffer and do some setup details."
  (alien-sel-list-mode)
  (setq -alien-sel-filter-stack nil)
  (setq -alien-sel-options options)
  (setq -alien-sel-filter "")
  (-alien-sel-apply-filter)
  (setq -alien-sel-index 0)
  (setq -alien-sel-prompt prompt)
  (setq -alien-sel-buffer (current-buffer))
  (select-window (display-buffer (current-buffer)))
  (setq cursor-in-non-selected-windows nil))

;; TODO: Make a function that takes an argument (the variable to
;; normalize), and use it also for first-visible-index and
;; last-visible index, in the render function.
(defun -alien-sel-normalize-index()
  "Checks the index for boundaries, so that movement functions
don't need to."
  (if (zerop (length -alien-sel-options-filtered))
      (setq -alien-sel-index nil)
    (when (null -alien-sel-index)
      (setq -alien-sel-index 0))
    (when (< -alien-sel-index 0)
      (setq -alien-sel-index 0))
    (when (>= -alien-sel-index (length -alien-sel-options-filtered))
      (setq -alien-sel-index (1- (length -alien-sel-options-filtered))))))


(defun -alien-sel-render-set-mode-line-filter-kind(modes)
  (concat 
   (propertize "prefix" 'face (if (first modes) 'bold 'shadow)) " "
   (propertize "substr" 'face (if (second modes) 'bold 'shadow)) " "
   (propertize "flex" 'face (if (third modes) 'bold 'shadow)) " "
   (propertize "regexp" 'face (if (fourth modes) 'bold 'shadow))))

;; TODO - Probably the filter stack will live better in the header line,
;; or event on the minibuffer, above the prompt.

(defun -alien-sel-render-set-mode-line()
  "Sets the modeline for the list buffer. It includes current
filter type and the filter stack"

  (let ((cycle-filter-key
         (key-description
          (first
           (where-is-internal
            'alien-sel-cycle-filter-type
            alien-sel-minibuffer-map))))
        (filter-type
         (-alien-sel-render-set-mode-line-filter-kind
          (cond
           ((eq alien-sel-filter-type 'prefix-substring-flex) '(t t t nil))
           ((eq alien-sel-filter-type 'prefix-substring) '(t t nil nil))
           ((eq alien-sel-filter-type 'prefix) '(t nil nil nil))
           ((eq alien-sel-filter-type 'regexp) '(nil nil nil t)))))
        (filter-stack-string
           (apply 'concat
                  (mapcar
                   (lambda(x)
                     (concat
                      (cond
                       ((eq (second x) 'prefix-substring-flex) "fl:")
                       ((eq (second x) 'prefix) "^:")
                       ((eq (second x) 'regexp) "re:")
                       (""))
                      "[" (first x) "] "))
                   -alien-sel-filter-stack))))
    (setq mode-line-format
          (list
           mode-line-front-space
           "[" cycle-filter-key "] "
           filter-type
           " { " filter-stack-string " }"
           mode-line-end-spaces))))
  
(defun -alien-sel-render()
  "Renders the selection buffer. Requires that the list buffer is
the current buffer. It erases current buffer, so watch out."
  (-alien-sel-normalize-index)
  (read-only-mode 0)    
  (-alien-sel-header-line)
  (erase-buffer)

  (if -alien-sel-index
      (let* ((index 0)
             face selected-point
             (first-index (- -alien-sel-index alien-sel-visible-item-max-count-before-selected))
             last-index)
        (when (< first-index 0)
          (setq first-index 0))
        (setq last-index (+ (1- alien-sel-visible-item-max-count) first-index))
        (when (>= last-index (length -alien-sel-options-filtered))
          (setq last-index (1- (length -alien-sel-options-filtered))))
        
        (if (> first-index 0)
            (insert (propertize (format "    ... %d more\n" first-index) 'face 'alien-sel-light-face))
          (insert "\n"))
        
        (dolist (o -alien-sel-options-filtered)
          (and
           (>= index first-index)
           (<= index last-index)
           (progn
             (if (= index -alien-sel-index)
                 (progn
                   (setq selected-point (point))
                   (setq overlay-arrow-position (point-marker))
                   (setq face 'match))
               (setq face nil))
             (let* ((item (propertize (format " %s\n" o)  'face face)))
               (-alien-sel-propertize-for-filter item -alien-sel-filter)
               (insert
                (if alien-sel-show-line-numbers
                    (concat " " (propertize (format "%d" index) 'face face) item)
                  item)))))
          (incf index))
        (when (< last-index (1- (length -alien-sel-options-filtered)))
          (insert (propertize (format "    ... %d more\n"
                                      (- (1- (length -alien-sel-options-filtered)) last-index))
                              'face 'alien-sel-light-face)))
        (goto-char selected-point))
    (insert "\n  ")
    (insert (propertize "[NO MATCH]" 'face 'isearch-fail))
    (insert "\n")
    (setq overlay-arrow-position nil))
  (-alien-sel-render-set-mode-line)
  (read-only-mode 1)
  ;; If window is too small, recenter to the top.
  (recenter (+ 2 alien-sel-visible-item-max-count-before-selected)))

(defmacro -alien-sel-with-selection-buffer(&rest body)
  "Make this selection buffer current, selects its window and runs `body'"
  `(with-current-buffer -alien-sel-buffer
     (with-selected-window (get-buffer-window)
       ,@body)))

(defmacro -alien-sel-command(fname documentation key &rest body)
  "Creates a function named -alien-sel-`fname', which runs body
  under `-alien-sel-with-selection-buffer' and finally calls
  `-alien-sel-render'."
  (let ((name (intern (concat "alien-sel-" (symbol-name fname)))))
    `(progn
       (defun ,name ()
         ,(concat documentation "\nDefined with `-alien-sel-command'.")
         (interactive)
         (-alien-sel-with-selection-buffer
          ,@body
          (-alien-sel-render)))
       (define-key alien-sel-minibuffer-map ,key ',name))))
     
(setq alien-sel-minibuffer-map
      (let ((map (make-sparse-keymap)))
        (set-keymap-parent map minibuffer-local-map)
        (define-key map [return] 'alien-sel-enter)
        (define-key map [(meta up)] 'alien-sel-switch-to-listing-buffer)
        map))

(-alien-sel-command
 cycle-filter-type "Cycles through available filter types" [(control ?+)]
                    (setq alien-sel-filter-type
                          (cond
                           ((eq alien-sel-filter-type 'prefix-substring-flex) 'regexp)
                           ((eq alien-sel-filter-type 'regexp) 'prefix-substring)
                           ((eq alien-sel-filter-type 'prefix-substring) 'prefix)
                           ('prefix-substring-flex) ))
                    (-alien-sel-apply-filter)
                    (setq -alien-sel-index 0))

(-alien-sel-command toggle-display-numbers "Toggles listing numbers." [(meta ?+)]
                    (setq alien-sel-show-line-numbers (not alien-sel-show-line-numbers)))

(-alien-sel-command move-up "Moves selection up." [up] (when -alien-sel-index (decf -alien-sel-index)))
(-alien-sel-command move-down "Moves selection down." [down] (when -alien-sel-index (incf -alien-sel-index)))
(-alien-sel-command page-down "Moves selection down a few lines." [next]
                    (when -alien-sel-index 
                      (incf -alien-sel-index
                            (- alien-sel-visible-item-max-count
                               alien-sel-visible-item-max-count-before-selected))))
(-alien-sel-command page-up "Moves selection up a few lines." [prior]
                    (when -alien-sel-index
                      (decf -alien-sel-index
                            (- alien-sel-visible-item-max-count
                               alien-sel-visible-item-max-count-before-selected))))
(-alien-sel-command move-to-start "Moves selection to first item." [home]
                    (when -alien-sel-index
                      (setq -alien-sel-index 0)))
(-alien-sel-command move-to-end "Moves selection to last item." [end]
                    (when -alien-sel-index
                      (setq -alien-sel-index (length -alien-sel-options-filtered))))

(-alien-sel-command enlarge-list "Increases the number of visible options." [S-down]
                    (incf alien-sel-visible-item-max-count 5))

(-alien-sel-command shorten-list "Decreases the number of visible options." [S-up]
                    (setq alien-sel-visible-item-max-count
                          (max 12 (- alien-sel-visible-item-max-count 5))))

(-alien-sel-command one-window "Deletes all other windows, so the list window takes the whole frame."
                    [(control ?c) ?1]
                    (delete-other-windows (get-buffer-window -alien-sel-buffer)))

(-alien-sel-command filter-stack-push
                    "Pushed current filter into stack, making
filtered list the default list until filter is poped from stack."
                    [(control return)]
                    (unless (string-empty-p -alien-sel-filter)
                      (add-to-list '-alien-sel-filter-stack
                                   (list -alien-sel-filter
                                         alien-sel-filter-type
                                         -alien-sel-options)
                                   nil
                                   ;; Don't allow duplicated filters (same text, same kind)
                                   (lambda(x y)
                                     (and (equal (first x) (first y))
                                          (equal (second x) (second y)))))
                      (setq -alien-sel-options -alien-sel-options-filtered)
                      (with-current-buffer -alien-sel-minibuffer-buffer
                        (delete-minibuffer-contents))))
                    
(-alien-sel-command filter-stack-pop
                    "Pops one filter from the filter stack."
                    [(meta return)]
                    (setq -alien-sel-options
                          (third (pop -alien-sel-filter-stack)))
                    (-alien-sel-apply-filter))

(-alien-sel-command degrade
                    "Falls back to completing-read" [(meta down)]
                    (setq -alien-sel-degraded t)
                    (exit-minibuffer))

                    
(defun alien-sel-switch-to-listing-buffer()
  "Selects the listing window"
  (interactive)
  (select-window (get-buffer-window -alien-sel-buffer)))
  
(defun alien-sel-enter()
  "Selects the current item and exits"
  (interactive)
  (when -alien-sel-index
    (let ((selected-text
           (-alien-sel-with-selection-buffer
            (nth -alien-sel-index -alien-sel-options-filtered))))
      (delete-minibuffer-contents)
      (insert selected-text)
      (let ((val (get-text-property 0 'alien-sel-val selected-text)))
        (when val
          (delete-minibuffer-contents)
          (insert val)))
      (exit-minibuffer))))

(defun -alien-sel-minibuffer-after-change(begin end len)
  "Hooked to `after-change-functions' for the minibuffer. Updates filter and renders"
  (setq -alien-sel-filter (minibuffer-contents))
  (-alien-sel-apply-filter)
  (setq -alien-sel-index 0)
  (-alien-sel-with-selection-buffer
    (-alien-sel-render)))

(defun -alien-sel-minibuffer-setup-hook()
  "Prepares the minibuffer."
  (setq -alien-sel-minibuffer-buffer (current-buffer))
  (add-hook 'after-change-functions
            '-alien-sel-minibuffer-after-change nil t))

(defun alien-sel(prompt options)
  "The entry point for alien-sel. Show the list options, with the
given prompt, and returns the selected option. If the selected
option text has the property `alien-sel-val', returns the value
of that property instead."
  (setq -alien-sel-degraded nil)
  (let* ((minibuffer-allow-text-properties t)
         (retval
          (save-window-excursion
            (with-current-buffer
                (get-buffer-create "*alien-sel*")
              (-alien-sel-setup-buffer prompt options)
              (-alien-sel-render)
              (unwind-protect
                  (progn
                    (add-hook 'minibuffer-setup-hook '-alien-sel-minibuffer-setup-hook)
                    (read-from-minibuffer
                     (-alien-sel-prompt prompt)
                     nil
                     alien-sel-minibuffer-map))
                (remove-hook 'minibuffer-setup-hook '-alien-sel-minibuffer-setup-hook)
                (with-current-buffer -alien-sel-minibuffer-buffer
                  (remove-hook 'after-change-functions
                               '-alien-sel-minibuffer-after-change t))
                (kill-buffer))))))
    (if -alien-sel-degraded
        (completing-read (concat prompt ": ") options nil t)
      retval)))

(defun alien-sel-back-to-minibuffer()
  "Reselects the minibuffer"
  (interactive)
  (select-window (active-minibuffer-window)))

(define-derived-mode alien-sel-list-mode special-mode "/A\\sel\\"
  "Mode for the listing buffer on alien-sel"
  (define-key alien-sel-list-mode-map [(meta down)] 'alien-sel-back-to-minibuffer)
  (define-key alien-sel-list-mode-map [(control ?g)] 'alien-sel-back-to-minibuffer))
  
