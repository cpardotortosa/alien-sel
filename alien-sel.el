;;; -*- coding: utf-8 -*-
;;; alien-sel.el --- A library for user selection/completion.

;; Copyright (C) 2019 Cecilio Pardo.

;; Author: Cecilio Pardo <cpardo@imayhem.com>
;; Keywords: selection, completion

(require 'dash)
(require 's)
(require 'subr-x)
(require 'cl)
(require 'bookmark)

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

;; Global variables with state information:

;; -alien-sel-options - The choices. Pushing filters changes this.
;; -alien-sel-options-filtered - The filtered choices.
;; -alien-sel-inline-frame - The child frame, when using one.
;; -alien-sel-with-header - The selection buffer has an informative header.
;; -alien-sel-with-modeline - The selection buffer has a modeline with filter information.
;; -alien-sel-filter-stack - The filters that have been pushed.
;; -alien-sel-filter - The current filter string.
;; -alien-sel-index - The currently selected index.
;; -alien-sel-prompt - The prompt.
;; -alien-sel-buffer - The selection buffer
;; -alien-sel-degraded - Flag set by the comannd alien-sel-degrade. When active, call completing-read after finishing.
;; -alien-sel-minibuffer-overlay - The overlay used to display filter information on the minibuffer. It is used when the
;;                                 selection buffer has no modeline.
;; -alien-sel-minibuffer-buffer - The minibuffer.

(defun -alien-sel-prompt(prompt &optional nocolon)
  "Builds the prompt from a string, adding the Alien logo"
  (format " %s %s%s"
          (propertize "/A\\" 'face 'alien-sel-logo-face )
          prompt
          (if nocolon "" ": ")))

(defun -alien-sel-header-line()
  "Sets the header line for the listing buffer. It includes the
prompt, and the number of items (passing filter/total)."
  (if -alien-sel-with-header
      (let* ((l1 (-alien-sel-prompt -alien-sel-prompt t))
             (l2 (propertize (format
                              " %d/%d/%d items"
                              alien-sel-visible-item-max-count
                              (length -alien-sel-options-filtered)                          
                              (length -alien-sel-options)))))
        (setq header-line-format (format " %s       %s" l1 l2)))))

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

(defun -alien-sel-normalize-inline-frame(frame)
  (let* ((p (window-absolute-pixel-position))
         (f (frame-edges nil 'inner-edges))
         (x (- (car p) (first f)))
         (y (- (cdr p) (second f))))
    ;; Move it to the cursor.
    (set-frame-position frame x y)
    (let* ((fchild (frame-edges frame 'outer-edges))
           (x-excess (- (third fchild) (third f)))
           (y-excess (- (fourth fchild) (fourth f))))
      ;; If too far right, move to the left.
      (when (> x-excess 0)
        (setq x (max 0 (- x x-excess)))
        (set-frame-position frame x y))
      (when (> y-excess 0)
        (set-frame-position frame x (max 0 (- y y-excess)))))))

(defun -alien-sel-inline-frame-parameters(type)
  `((child-frame-parameters .
                            ,(append
                              '((undecorated . t)
                                (vertical-scroll-bars . nil)
                                (border-width . 1)
                                (left-fringe . 0)
                                (right-fringe . 0)
                                (minibuffer . nil)
                                (width . 55)
                                (alpha . 90))

                              (cond
                                ((eq type 'minimal)
                                 '((height . 15)))
                                ('((height . 17)))
                                
                                )))))

;; TODO: Should pass (minibuffer . [minibuffer-window])?
;; TODO: Probably pass (minibuffer-exit . t), instead of deleting the frame.
;; TODO: Check what happens if scrollbars are enabled.
(defun -alien-sel-display-buffer(buf inline)
  (if (not inline)
      (display-buffer buf)
    (let ((window
           (display-buffer-in-child-frame
            buf
            (-alien-sel-inline-frame-parameters inline))))
      (setq -alien-sel-inline-frame (window-frame window))

      (cond
       ((eq inline 'minimal)
        (setq -alien-sel-with-header nil)
        (setq -alien-sel-with-modeline nil)
        ))
      
      (-alien-sel-normalize-inline-frame -alien-sel-inline-frame)
      window)))

  
(defun -alien-sel-setup-buffer(prompt options &optional inline)
  "After creating the selection buffer, sets the global
variables, displays the buffer and do some setup details."
  (alien-sel-list-mode)
  (let ((inhibit-message t))
    (toggle-truncate-lines 1))
  (setq -alien-sel-filter-stack nil)
  (setq -alien-sel-options options)
  (setq -alien-sel-filter "")
  (-alien-sel-apply-filter)
  (setq -alien-sel-index 0)
  (setq -alien-sel-prompt prompt)
  (setq -alien-sel-buffer (current-buffer))

  (setq -alien-sel-with-header t)
  (setq -alien-sel-with-modeline t)
  (select-window (-alien-sel-display-buffer (current-buffer) inline))
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

(defun -alien-sel-make-mode-line-format()
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
    (list
     mode-line-front-space
     "[" cycle-filter-key "] "
     filter-type
     " { " filter-stack-string " }"
     mode-line-end-spaces)))

;; TODO - Probably the filter stack will live better in the header line,
;; or event on the minibuffer, above the prompt.
(defun -alien-sel-render-set-mode-line()
  "Sets the modeline for the list buffer. It includes current
filter type and the filter stack"
  (if -alien-sel-with-modeline
      (setq mode-line-format
            (-alien-sel-make-mode-line-format))
    (setq
     mode-line-format nil)))

(defun -alien-sel-render-item(o index face max-length)
  (if alien-sel-show-line-numbers
      (insert (propertize (format " %d" index) 'face face)))
  (insert (propertize " " 'face face))
  (let ((item
         (if face
             (propertize o 'face face)
           o))
        (subtext (get-text-property 0 'alien-sel-subtext o)))
    (-alien-sel-propertize-for-filter item -alien-sel-filter)
    (insert item)
    (if subtext
        (insert
         (propertize " " 'face face)
         (propertize " " 'face face 'display `(space .(:align-to ,max-length)))
         (propertize subtext 'face
                     (if face
                         `(:foreground "#808080" :slant oblique :inherit ,face)
                         `(:foreground "#808080" :slant oblique)) ))))
  (insert (propertize "\n" 'face face)))

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
             selected-detail-text
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
        
        (let ((max-length 0))
          (dolist (o -alien-sel-options-filtered)
            (and
             (>= index first-index)
             (<= index last-index)
             (setq max-length (max max-length (length o))))
            (incf index))
          ;; add some chars for spacing
          (setq max-length (+ 7 max-length))
          ;; align to multiples of 7. Not related to any other 7.
          (setq max-length (* 7 (ceiling (/ (float max-length) 7.0))))
          (setq index 0)
          (dolist (o -alien-sel-options-filtered)
            (and
             (>= index first-index)
             (<= index last-index)
             (progn
               (if (= index -alien-sel-index)
                   (progn
                     (setq selected-point (point))
                     (setq selected-detail-text (get-text-property 0 'alien-sel-detail o))
                     (unless -alien-sel-inline-frame
                       (setq overlay-arrow-position (point-marker)))
                     (setq face 'highlight))
                 (setq face 'nil))
               (-alien-sel-render-item o index face max-length)))
            (incf index)))

        
        (when (< last-index (1- (length -alien-sel-options-filtered)))
          (insert (propertize (format "    ... %d more\n"
                                      (- (1- (length -alien-sel-options-filtered)) last-index))
                              'face 'alien-sel-light-face)))
        
        (when selected-detail-text
          (insert "\n -------------------------------------------\n")
          (let ((start (point)))
            (insert selected-detail-text)
            (indent-region start (point) 4))
          (insert "\n -------------------------------------------\n"))

        (goto-char selected-point))
    (insert "\n  ")
    (insert (propertize "[NO MATCH]" 'face 'isearch-fail))
    (insert "\n")
    (setq overlay-arrow-position nil))
  (-alien-sel-render-set-mode-line)
  (read-only-mode 1)
  ;; If window is too small, recenter to the top.
  (recenter (+ 2 alien-sel-visible-item-max-count-before-selected))

  ;; If there is no modeline, show the filter configuration in the minibuffer.
  (-alien-sel-add-filter-info-in-minibuffer))

(defmacro -alien-sel-with-selection-buffer(&rest body)
  "Make this selection buffer current, selects its window and runs `body'"
  `(with-current-buffer -alien-sel-buffer
     (with-selected-window (get-buffer-window nil t)
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
       (let ((key-or-keys ,key))
         (--each (if (listp key-or-keys)
                     key-or-keys
                   (list key-or-keys))
           (define-key alien-sel-minibuffer-map it ',name))))))
     
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
                    (when -alien-sel-filter-stack
                      (setq -alien-sel-options
                            (third (pop -alien-sel-filter-stack))))
                    (-alien-sel-apply-filter))

(-alien-sel-command degrade
                    "Falls back to completing-read" [(meta down)]
                    (setq -alien-sel-degraded t)
                    (exit-minibuffer))

(defun alien-sel-switch-to-listing-buffer()
  "Selects the listing window"
  (interactive)
  (select-window (get-buffer-window -alien-sel-buffer t)))
  
(defun alien-sel-enter()
  "Selects the current item and exits"
  (interactive)
  (when -alien-sel-index
    (let ((selected-text
           (-alien-sel-with-selection-buffer
            (nth -alien-sel-index -alien-sel-options-filtered))))
      (delete-minibuffer-contents)
      (setq alien-sel-returned-item selected-text)
      (remove-text-properties 0 (length selected-text)
                              '(alien-sel-score nil)
                              selected-text)
      (setq alien-sel-returned-index (--find-index
                                      (equal-including-properties selected-text it)
                                      -alien-sel-options))
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

(defun -alien-sel-add-filter-info-in-minibuffer()
  "When using minimal popup frame for completion list, the filter
  info is show in the minibuffer. This functions puts it there"
  (unless -alien-sel-with-modeline
    (with-current-buffer -alien-sel-minibuffer-buffer
      (when -alien-sel-minibuffer-overlay
        (delete-overlay -alien-sel-minibuffer-overlay))
      (setq -alien-sel-minibuffer-overlay
            (make-overlay (point-min) (1+ (point-min))))
      (overlay-put -alien-sel-minibuffer-overlay 'before-string
                   (concat (format-mode-line (-alien-sel-make-mode-line-format)) "\n")))))

(defun -alien-sel-minibuffer-setup-hook()
  "Prepares the minibuffer."
  (setq -alien-sel-minibuffer-buffer (current-buffer))
  (setq -alien-sel-minibuffer-overlay nil)
  (add-hook 'after-change-functions
            '-alien-sel-minibuffer-after-change nil t)
  (-alien-sel-add-filter-info-in-minibuffer))

(defun alien-sel(prompt options &optional inline)
  "The entry point for alien-sel. Shows the list of options, with
the given prompt, and returns the selected option. 

If the selected option text has the property `alien-sel-val', returns
the value of that property instead. 

If inline is not nil, then show the list in its own popup,
undecorated frame. If it is 'minimal, shows a frame without
header and mode line. In this case, the filter information is
presented in the minibuffer."

  (setq -alien-sel-degraded nil)
  (let* ((minibuffer-allow-text-properties t)
         (retval
          (save-window-excursion
            (with-current-buffer
                (get-buffer-create "*alien-sel*")
              (setq -alien-sel-inline-frame nil)
              (-alien-sel-setup-buffer prompt options inline)
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
                (kill-buffer)
                (if -alien-sel-inline-frame
                    (delete-frame -alien-sel-inline-frame)))))))
    (if -alien-sel-degraded
        (completing-read (concat prompt ": ") options nil t)
      retval)))

(defun alien-sel-back-to-minibuffer()
  "Reselects the minibuffer"
  (interactive)
  (select-window (active-minibuffer-window)))

(define-derived-mode alien-sel-list-mode special-mode "/A\\sel\\"
  "Mode for the listing buffer on alien-sel"
  (define-key alien-sel-list-mode-map [wheel-up] 'alien-sel-page-up)
  (define-key alien-sel-list-mode-map [wheel-down] 'alien-sel-page-down)
  (define-key alien-sel-list-mode-map [(meta down)] 'alien-sel-back-to-minibuffer)
  (define-key alien-sel-list-mode-map [(control ?g)] 'alien-sel-back-to-minibuffer))


(defun alien-sel-test-1(popup)
  (message "Your choice: %s"
           (alien-sel "Pick a color"
                      (--map
                       (let ((rgb (color-name-to-rgb it)))
                         (propertize it
                                     'alien-sel-subtext
                                     (color-rgb-to-hex (first rgb) (second rgb) (third rgb) 2)
                                     'alien-sel-detail
                                     (propertize "[                 ]"
                                                 'face `(:background ,it :foreground ,it))))
                       (defined-colors))
                      popup)))

(defun alien-sel-test()
  (interactive)
  (alien-sel-test-1 nil))

(defun alien-sel-test-popup()
  (interactive)
  (alien-sel-test-1 t))

(defun alien-sel-test-minimal-popup()
  (interactive)
  (alien-sel-test-1 'minimal))
   
(defun alien-sel-bookmark()
    "Pick a bookmark with alien-sel"
    (interactive)
    (bookmark-maybe-load-default-file)
    (bookmark-jump
     (bookmark-get-bookmark
      (alien-sel
       "Bookmark"
       (--map
        (propertize
         (bookmark-name-from-full-record it)
         'alien-sel-subtext (bookmark-get-filename it))
        bookmark-alist))))
)
