;;; nano-org.el --- N Λ N O org mode -*- lexical-binding: t -*-

;; Copyright (C) 2024 Nicolas P. Rougier
;; Maintainer: Nicolas P. Rougier <Nicolas.Rougier@inria.fr>

;; This file is not part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <https://www.gnu.org/licenses/>.

(require 'org)
(require 'nano-theme)


(defun nano-org--edit (_win position direction)
  "This function toggle font-lock at position, depending on
direction."

  (let ((beg (if (eq direction 'entered)
                 (previous-property-change (+ (point) 1))
               (previous-property-change (+ position 1))))
        (end (if (eq direction 'entered)
                 (next-property-change (point))
               (next-property-change position))))
    (if (eq direction 'left)
        (font-lock-flush beg (1+ end) )
      (if (and (not view-read-only) (not buffer-read-only))
          (font-lock-unfontify-region beg (1+ end))))))


(defun nano-org-archived-p ()
  "Returns non-nil if point is on an archived header."

  (member org-archive-tag (org-get-tags nil t)))


(defun nano-org-folded-p (&optional types)
  "Returns non-nil if point is on a folded element whose type is
specified by TYPES that defaults to '(heading drawer item block)."

  (let ((types (or types '(heading drawer item block))))
    (and (or (when (memq 'heading types) (org-at-heading-p))
             (when (memq 'drawer types) (org-at-drawer-p))
             (when (memq 'item types) (org-at-item-p))
             (when (memq 'block types) (org-at-block-p)))
         (invisible-p (point-at-eol)))))


(defun nano-org--timestamp ()
  "Prettify timestamps."

  (let* ((beg (match-beginning 1))
         (end (match-end 1))
         (keyword (match-string 2))
         (keyword (when (stringp keyword)
               (string-trim (substring-no-properties keyword))))
         (is-archived (nano-org-archived-p))
         (is-deadline (string= keyword "DEADLINE:"))
         (is-scheduled (string= keyword "SCHEDULED:"))
         (tbeg (match-beginning 4))
         (tend (match-end 4))
         (active t)
         (keymap (define-keymap
                   "S-<up>"   (lambda ()
                                (interactive)
                                (let ((org-time-stamp-rounding-minutes '(0 15 30 45)))
                                  (org-timestamp-change +15 'minute)))
                   "S-<down>" (lambda ()
                                (interactive)
                                (let ((org-time-stamp-rounding-minutes '(0 15 30 45)))
                                  (org-timestamp-change -15 'minute)))))
         (date-face (cond (is-archived  `(:inherit (nano-faded nano-subtle)
					  :overline ,(face-background 'nano-default)))
                          (active       `(:inherit (nano-default bold nano-subtle)
					  :overline ,(face-background 'nano-default)))
                          (t            `(:inherit (nano-faded bold nano-subtle)
					  :overline ,(face-background 'nano-default)))))
         (time-face (cond (is-archived  `(:inherit (nano-faded nano-subtle)
					  :overline ,(face-background 'nano-default)))
                          (is-scheduled  `(:inherit (nano-popout-i)
					  :overline ,(face-background 'nano-default)))
                          (is-deadline  `(:inherit (nano-critical-i)
					  :overline ,(face-background 'nano-default)))
                          (t            `(:inherit (nano-default-i bold)
					  :overline ,(face-background 'nano-default))))))
    (remove-list-of-text-properties beg end '(display))
    (add-text-properties beg end `(keymap ,keymap))
    (let* ((time (save-match-data
		   (encode-time
		    (org-fix-decoded-time
		     (org-parse-time-string
		      (buffer-substring beg end))))))
	   (date-str (format-time-string " %^b %d " time))
	   (time-str (format-time-string " %H:%M " time)))
      ;; year-month-day
      (add-text-properties beg (if (eq tbeg tend) end tbeg)
			   `(face ,date-face display ,date-str))
      ;; hour:minute
      (unless (eq tbeg tend)
	(add-text-properties tbeg end
			     `(face ,time-face display ,time-str))))
    (put-text-property beg (1+ beg) 'display " ")
    (put-text-property (1- end) end 'display " ")
    ;; year-month-day
    (put-text-property beg (if (eq tbeg tend) end tbeg) 'face date-face)
    ;; hour:minute
    (unless (eq tbeg tend)
      (put-text-property (1- tbeg) tbeg 'display
			 (string (char-before tbeg) ?\s))
      (put-text-property tbeg end 'face time-face))))

(defun nano-org--todo ()
  "TODO keyword highlighting"

  (let* ((state (substring-no-properties (match-string 1)))
	 (inherit-face
	  (lambda (state)
	    (cond ((string= state "TODO") 'nano-salient-i)
		  ((string= state "DOING") 'nano-popout-i)
		  ((string= state "DONE") 'nano-faded-i)
		  ((string= state "HOLD") 'nano-popout-i)
		  ((string= state "NOPE") 'nano-faded-i)
		  (t 'nano-default))))
         (state-face `(:inherit ,(funcall inherit-face state)
		       :height 110
		       :weight medium
		       :overline ,(face-background 'default)))
	 (state (concat " " state " ")))
    (propertize state 'face state-face)))

(defun nano-org--priority ()
  "TODO priority tag highlighting"

  (let* ((tag (substring-no-properties (match-string 1)))
	 (level (substring-no-properties (match-string 2)))
	 (inherit-face
	  (lambda (prio)
	    (cond ((< prio 3) 'nano-critical)
		  ((< prio 6) 'nano-popout)
		  ((< prio 10) 'nano-faded)
		  (t 'nano-faded))))
         (level-face `(:inherit ,(funcall inherit-face (string-to-number level))
		       :height 110
		       :weight medium
		       :box (:line-width 1)))
	 (tag (concat " " level " ")))
    (propertize tag 'face level-face)))

(defun nano-org--properties ()
  "Properties drawer prefix depending on folding state"

  (if (nano-org-folded-p) " " "┌ "))

(defun nano-org--logbook ()
  "Logbook drawer prefix depending on folding state"

  (if (nano-org-folded-p) " " "┌ "))

(defun nano-org--ul-list ()
  "Unordered list prefix depending on folding state"

  (if (nano-org-folded-p) "  " nil))

(defun nano-org--ol-list ()
  "Orered list prefix depending on folding state"

  (if (nano-org-folded-p) " " nil))

(defun nano-org--user ()
  "Pretty format for user"

  (let* ((user (substring-no-properties (match-string 1)))
         (user (string-replace "@" " " user)))
    (propertize user 'face (if (nano-org-archived-p)
                               'nano-faded
                             'nano-salient)
                'pointer 'hand
                'mouse-face (when (not (nano-org-archived-p))
                              '(:inherit (nano-subtle bold))))))

(defvar nano-org--todo-re
  "^\\*+[\\\t ]+\\(TODO\\|DONE\\|DOING\\|HOLD\\|NOPE\\)")

(defvar nano-org--priority-re
  "^\\*+[\\\t ]+\\(?:[A-Z]* \\)\\(\\[#\\([0-9]+\\)\\]\\)")

(defvar nano-org--timestamp-re
  (concat 
   "\\("                                       ; Group 1: whole timestamp
   "\\("                                       ; Group 2: SCHEDULED / DEADLINE (optional)
   "\\(?:SCHEDULED:\\|DEADLINE:\\)[\\\t ]+"    ;
   "\\)?"                                      ;
   "\\(?:<\\|\\[\\)"                           ; Anonymous group for < or [
   "\\("                                       ; Group 3 start: date (mandatory)
   "[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}"    ;   YYYY-MM-DD (mandatory)
   ;; "\\(?: [[:word:]]+\\)?"                     ;   day name (optional)
   ;; "\\(?: [.+-]+[0-9ymwdh/]+\\)*"              ;   repeater (optional)
   "\\)"                                       ; Group 3 end
   "\\("                                       ; Group 4 start (optional): time
   "\\(?: [0-9:-]+\\)?"                        ;   HH:MM (optional)
   "\\(?: [.+-]+[0-9ymwdh/]+\\)*"              ;   repeater (optional)
   "\\)"                                       ; Group 4 end
   "\\(?:>\\|\\]\\)"                           ; Anonynous group for > or ]
   "\\)"))                                     ; Group 1 end

(defvar nano-org--drawer-properties-re
  "^\\(:\\)PROPERTIES:")                              ;; Group 1 for :[PROPERTIES:]

(defvar nano-org--drawer-logbook-re
  "^\\(:\\)LOGBOOK:")                                 ;; Group 1 for :[LOGBOOK:]

(defvar nano-org--drawer-closed-re
  "^\\(CLOSED:\\)")                                   ;; Group 1 for CLOSED:

(defvar nano-org--drawer-content-re
  "^\\(:\\)[a-zA-Z]+:")                               ;; Group 1 for :[XXX:]

(defvar nano-org--drawer-clock-re
  "^\\(CLOCK:\\)")                                    ;; Group 1 for CLOCK:

(defvar nano-org--drawer-end-re
  "^\\(:\\)END:")                                     ;; Group 1 for :[END:]

(defvar nano-org--ul-list-re
  "^\\(- \\)")                                        ;; Group 1 for -

(defvar nano-org--ol-list-re
  "^\\([0-9].\\)")                                    ;; Group 1 for #.

(defvar nano-org--user-re
  "\\(@[a-zA-Z]+\\)")                                 ;; Group 1 for @XXX

(defun org-nano--cycle-hook (&rest args)
  (font-lock-update))


(defun nano-org ()
  "NANO org mode (WIP)"

  (interactive)
  (font-lock-add-keywords nil
     `((,nano-org--todo-re             1 `(face nil display ,(nano-org--todo)))
       (,nano-org--priority-re         1 `(face nil display ,(nano-org--priority)))
       (,nano-org--drawer-logbook-re    1 `(face nil display "┌ "))
       (,nano-org--drawer-content-re    1 `(face nil display "│ "))
       (,nano-org--drawer-end-re        1 `(face nil display "└ "))
       (,nano-org--drawer-clock-re      1 `(face nil display "│  "))
       (,nano-org--drawer-properties-re 1 `(face nil display ,(nano-org--properties)))
       (,nano-org--drawer-logbook-re    1 `(face nil display ,(nano-org--logbook)))
       (,nano-org--drawer-closed-re     1 `(face nil display " "))
       (,nano-org--user-re              1 `(face nil display ,(nano-org--user)))
       (,nano-org--ul-list-re           1 `(face nil display ,(nano-org--ul-list)))
       (,nano-org--ol-list-re           1 `(face nil display ,(nano-org--ol-list))))
     'append)

  (add-hook 'org-cycle-hook #'org-nano--cycle-hook)
  (advice-add 'org-fold-hide-drawer-toggle :after
              #'org-nano--cycle-hook)
  (cursor-sensor-mode -1)
  (font-lock-update))

(provide 'nano-org)
