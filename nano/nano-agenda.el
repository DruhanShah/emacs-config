;;; nano-agenda.el --- N Λ N O agenda -*- lexical-binding: t -*-

;; Copyright (C) 2021-2025 Free Software Foundation, Inc.

;; Maintainer: Nicolas P. Rougier <Nicolas.Rougier@inria.fr>
;; URL: https://github.com/rougier/nano-agenda
;; Version: 0.4.0
;; Package-Requires: ((emacs "27.1"))
;; Keywords: convenience, org-mode, org-agenda

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

;;; Commentary:
;;
;; N Λ N O agenda is a minimal view of your org agenda files. It
;; displays a calendar view of current month (or the month
;; corresponding to the current selected date) alongside a view of
;; your agenda displaying timestamped entries. The agenda can be
;; navigated using arrows keys and killed using "q", "return" or
;; "escape".
;;
;; Usage example:
;;
;; M-x: nano-agenda
;;
;;; NEWS:
;;
;; Version 0.5.0
;; - Get rid of svg-lib dependency
;; - Removed two line header
;;
;; Version 0.4.0
;; - Full rewrite
;; - Two line header (tab-line + header-line)
;;
;; Version 0.3.2
;; - Possibiliy to select and edit entries
;;
;; Version 0.3.1
;; - Specific face and marker for deadlines
;;
;; Version 0.3.0
;; - Use of a single buffer for calendar + agenda
;; - Added palette choice for colorizing the calendar
;;
;; Version 0.2.2
;; - Better entries sorting
;; - Include non timestamped entries
;;
;; Version 0.2.1
;; - Removed ts requirements
;;
;; Version 0.2
;; - Removed ts (MELPA) dependency
;;
;; Version 0.1
;; - Submission to ELPA
;;
;;; Code
(require 'holidays)
(require 'org-agenda)
(require 'hl-line)

(defgroup nano-agenda nil
  "N Λ N O Agenda"
  :group 'nano)

(defgroup nano-agenda-faces nil
  "N Λ N O Agenda"
  :group 'nano-agenda)

(defcustom nano-agenda-sort-predicate #'nano-agenda--entry-sort
  "Predicate function to sort entries."
  :group 'nano-agenda
  :type 'function)

(defcustom nano-agenda-filter-predicate #'nano-agenda--entry-filter
  "Predicate function to filter out entries."
  :group 'nano-agenda
  :type 'function)

(defcustom nano-agenda-conflict-predicate #'nano-agenda--entry-conflict
  "Predicate function to check if two entries conflict (e.g. time overlap)"
  :group 'nano-agenda
  :type 'function)

(defcustom nano-agenda-occupancy-predicate #'nano-agenda--entry-occupancy
  "Predicate function to measure entry occupancy"
  :group 'nano-agenda
  :type 'function)

(defcustom nano-agenda-view-mode 'week
  "Agenda view mode (day or week)"
  :group 'nano-agenda
  :type '(choice (const day) (const week)))

(defcustom nano-agenda-header-show nil
  "Temporary debug variable"
  :group 'nano-agenda
  :type 'boolean)

(defcustom nano-agenda-tags-align t
  "Whether to align tags on the right"

  :group 'nano-agenda
  :type 'boolean)

(defface nano-agenda-default
  '((t :inherit default))
  "Default face (for casual day)"
  :group 'nano-agenda-faces)

(defface nano-agenda-header-title
  '((t :inherit bold))
  "Agenda header (title)"
  :group 'nano-agenda-faces)

(defface nano-agenda-header-subtitle
  '((t :inherit font-lock-comment-face))
  "Agenda header (subtitle)"
  :group 'nano-agenda-faces)

(defface nano-agenda-calendar-header-month
  '((t :inherit (nano-strong nano-subtle)))
  "Day name face (on second line)"
  :group 'nano-agenda-faces)

(defface nano-agenda-calendar-header-days
  '((t :inherit bold))
  "Month name face (on first line)"
  :group 'nano-agenda-faces)

(defface nano-agenda-calendar-default
  '((t :inherit default))
  "Current day face"
  :group 'nano-agenda-faces)

(defface nano-agenda-calendar-today
  '((t :box (:line-width (-1 . -1) :style nil)
       :inherit bold))
  "Current day face"
  :group 'nano-agenda-faces)

(defface nano-agenda-calendar-selected
  '((t :inherit (nano-salient-i nano-strong)))
  "Face for the selected day"
  :group 'nano-agenda-faces)

(defface nano-agenda-calendar-offday
  `((t :foreground ,(face-background 'hl-line nil 'default)))
  "Face for days outside curent month."
  :group 'nano-agenda-faces)

(defface nano-agenda-calendar-weekend
  '((t :inherit font-lock-comment-face))
  "Weekend face"
  :group 'nano-agenda-faces)

(defface nano-agenda-calendar-holidays
  '((t :inherit font-lock-comment-face))
  "Holidays face"
  :group 'nano-agenda-faces)

;; See https://material.io/design/color/the-color-system.html
(defvar nano-agenda-palettes
  '((red         . ("#FFEBEE" "#FFCDD2" "#EF9A9A" "#E57373" "#EF5350"
                    "#F44336" "#E53935" "#D32F2F" "#C62828" "#B71C1C"))
    (pink        . ("#FCE4EC" "#F8BBD0" "#F48FB1" "#F06292" "#EC407A"
                    "#E91E63" "#D81B60" "#C2185B" "#AD1457" "#880E4F"))
    (purple      . ("#F3E5F5" "#E1BEE7" "#CE93D8" "#BA68C8" "#AB47BC"
                    "#9C27B0" "#8E24AA" "#7B1FA2" "#6A1B9A" "#4A148C"))
    (deep-purple . ("#EDE7F6" "#D1C4E9" "#B39DDB" "#9575CD" "#7E57C2"
                    "#673AB7" "#5E35B1" "#512DA8" "#4527A0" "#311B92"))
    (indigo      . ("#E8EAF6" "#C5CAE9" "#9FA8DA" "#7986CB" "#5C6BC0"
                    "#3F51B5" "#3949AB" "#303F9F" "#283593" "#1A237E"))
    (blue        . ("#E3F2FD" "#BBDEFB" "#90CAF9" "#64B5F6" "#42A5F5"
                    "#2196F3" "#1E88E5" "#1976D2" "#1565C0" "#0D47A1"))
    (light-blue  . ("#E1F5FE" "#B3E5FC" "#81D4FA" "#4FC3F7" "#29B6F6"
                    "#03A9F4" "#039BE5" "#0288D1" "#0277BD" "#01579B"))
    (cyan        . ("#E0F7FA" "#B2EBF2" "#80DEEA" "#4DD0E1" "#26C6DA"
                    "#00BCD4" "#00ACC1" "#0097A7" "#00838F" "#006064"))
    (teal        . ("#E0F2F1" "#B2DFDB" "#80CBC4" "#4DB6AC" "#26A69A"
                    "#009688" "#00897B" "#00796B" "#00695C" "#004D40"))
    (green       . ("#E8F5E9" "#C8E6C9" "#A5D6A7" "#81C784" "#66BB6A"
                    "#4CAF50" "#43A047" "#388E3C" "#2E7D32" "#1B5E20"))
    (light-green . ("#F1F8E9" "#DCEDC8" "#C5E1A5" "#AED581" "#9CCC65"
                    "#8BC34A" "#7CB342" "#689F38" "#558B2F" "#33691E"))
    (lime        . ("#F9FBE7" "#F0F4C3" "#E6EE9C" "#DCE775" "#D4E157"
                    "#CDDC39" "#C0CA33" "#AFB42B" "#9E9D24" "#827717"))
    (yellow      . ("#FFFDE7" "#FFF9C4" "#FFF59D" "#FFF176" "#FFEE58"
                    "#FFEB3B" "#FDD835" "#FBC02D" "#F9A825" "#F57F17"))
    (amber       . ("#FFF8E1" "#FFECB3" "#FFE082" "#FFD54F" "#FFCA28"
                    "#FFC107" "#FFB300" "#FFA000" "#FF8F00" "#FF6F00"))
    (orange      . ("#FFF3E0" "#FFE0B2" "#FFCC80" "#FFB74D" "#FFA726"
                    "#FF9800" "#FB8C00" "#F57C00" "#EF6C00" "#E65100"))
    (deep-orange . ("#FBE9E7" "#FFCCBC" "#FFAB91" "#FF8A65" "#FF7043"
                    "#FF5722" "#F4511E" "#E64A19" "#D84315" "#BF360C"))
    (brown       . ("#EFEBE9" "#D7CCC8" "#BCAAA4" "#A1887F" "#8D6E63"
                    "#795548" "#6D4C41" "#5D4037" "#4E342E" "#3E2723"))
    (grey        . ("#FAFAFA" "#F5F5F5" "#EEEEEE" "#E0E0E0" "#BDBDBD"
                    "#9E9E9E" "#757575" "#616161" "#424242" "#212121"))
    (blue-grey   . ("#ECEFF1" "#CFD8DC" "#B0BEC5" "#90A4AE" "#78909C"
                    "#607D8B" "#546E7A" "#455A64" "#37474F" "#263238"))
    (viridis     . ("#fde725" "#b5de2b" "#6ece58" "#35b779" "#1f9e89"
                    "#26828e" "#31688e" "#3e4989" "#482878" "#440154"))
    (magma       . ("#fcfdbf" "#feca8d" "#fd9668" "#f1605d" "#cd4071"
                    "#9e2f7f" "#721f81" "#440f76" "#180f3d" "#000004"))
    (inferno     . ("#fcffa4" "#f7d13d" "#fb9b06" "#ed6925" "#cf4446"
                    "#a52c60" "#781c6d" "#4a0c6b" "#1b0c41" "#000004"))))

(defun nano-agenda-color-luminance (color)
  "Calculate the relative luminance of a color string (e.g. \"#ffaa00\", \"blue\").
Return a value between 0 and 1."
  (let* ((values (x-color-values color))
         (red (/ (car values) 256.0))
         (green (/ (cadr values) 256.0))
         (blue (/ (caddr values) 256.0)))
    (/ (+ (* .2126 red) (* .7152 green) (* .0722 blue)) 255)))

(defcustom nano-agenda-palette 'blue-grey
  (concat
   "Background colors to use to highlight a day in calendar view according to occupancy.\n\n"
   (mapconcat (lambda (palette)
                (let* ((name (symbol-name (car palette)))
                       (colors (cdr palette))
                       (colors (mapconcat
                                (lambda (index)
                                  (let* ((background (nth (1- index) colors))
                                         (foreground (if (< (nano-agenda-color-luminance background) 0.5)
                                                        "white" "black")))
                                    (propertize (format "%2d " index)
                                                'face `(:foreground ,foreground
                                                        :background ,background))))
                                (number-sequence 1 (length colors))
                                "")))
                  (concat (format "%-12s" name) " : " colors "\n")))
           nano-agenda-palettes ""))
  :type `(choice (const red)    (const pink)  (const purple)      (const deep-purple)
                 (const indigo) (const blue)  (const light-blue)  (const cyan)
                 (const teal)   (const green) (const light-green) (const lime)
                 (const yellow) (const amber) (const orange)      (const deep-orange)
                 (const brown)  (const grey)  (const blue-grey)
                 (const viridis) (const magma) (const inferno))
  :group 'nano-agenda-faces)

(defvar nano-agenda-update-hook nil
  "Normal hook run after agenda is updated")

(defvar nano-agenda-date nil
  "Active date when building the agenda")

(defvar nano-agenda-buffer-name "*nano-agenda*"
  "Name of the agenda buffer")

(defvar nano-agenda--entry-window nil
  "Window used for viewing an entry")

(defvar nano-agenda--entry-marker nil
  "Active entry (marker) when navigating the agenda")

(defvar nano-agenda--entry-is-now nil
  "Indicate whether an entry is happening now")

(defvar nano-agenda--entry-index 0
  "Active entry (index) when navigating the agenda")

(defun nano-agenda--entry-sort (entry-1 entry-2)
  "Return t if ENTRY-1 < ENTRY-2 according to their respective `time-of-day'"

  (let ((time-1 (get-text-property 0 'time-of-day entry-1))
        (time-2 (get-text-property 0 'time-of-day entry-2)))
    (cond ((not time-1) t)
          ((not time-2) nil)
          (t (< time-1 time-2)))))

(defun nano-agenda--entry-filter (entry)
  "Return t if ENTRY should be included in the agenda"

  (let ((type (get-text-property 0 'type entry)))
    (and (not (string-equal type "upcoming-deadline"))
         (not (string-search ":CANCELLED:" entry)))))

(defun nano-agenda--entry-conflict (entry-1 entry-2)
   "Check if date ranges ITEM-1 and ITEM-2 overlap."

   (when-let* ((date-1 (nano-agenda--entry-time entry-1))
               (date-2 (nano-agenda--entry-time entry-2))
               (beg-1 (car date-1))
               (end-1 (cdr date-1))
               (beg-2 (car date-2))
               (end-2 (cdr date-2))
               (conflict (cond ((time-equal-p beg-1 beg-2) t)
                               ((time-equal-p end-1 beg-2) nil)
                               ((time-equal-p end-2 beg-1) nil)
                               ((and (time-less-p beg-1 beg-2)
                                     (time-less-p beg-2 end-1)) t)
                               ((and (time-less-p end-2 end-1)
                                     (time-less-p beg-1 end-2)) t))))
     conflict))

(defun nano-agenda--entry-occupancy (entry)
  "Return occupancy for given ENTRY"

  (if (funcall #'nano-agenda--entry-filter entry)
    (let ((time (nano-agenda--entry-time entry)))
      (if (and (car time) (cdr time))
          1
        0))
    0))

(defvar nano-agenda--holidays nil
  "Cached list of holidays (indexed by date as (month day year))")

(defun nano-agenda-holidays (org-date &optional force-update)
  "Return holiday for given DATE as (month day year) and force
update if FORCE-UPDATE is t"

  (when (or (not (assoc org-date nano-agenda--holidays)) force-update)
    (add-to-list 'nano-agenda--holidays (cons org-date (car (calendar-check-holidays org-date)))))
  (cdr (assoc org-date nano-agenda--holidays)))

(defvar nano-agenda--anniversaries nil
  "Cached list of anniversaries (indexed by date as (month day year))")

(defun nano-agenda-anniversaries (org-date &optional force-update)
  "Return anniversaries for given DATE as (month day year) and force
update if FORCE-UPDATE is t."

  (when (or (not (assoc org-date nano-agenda--anniversaries)) force-update)
    (let ((diary (catch 'found
                   (dolist (file (org-agenda-files))
                     (dolist (entry (org-agenda-get-day-entries file org-date :sexp))
                       (if (nano-agenda--entry-filter entry)
                           (let* ((todo (or (get-text-property 0 'todo-state entry) ""))
                                  (text (get-text-property 0 'txt entry))
                                  (text (replace-regexp-in-string ":.*:" "" text))
                                  (text (replace-regexp-in-string todo "" text))
                                  (text (org-link-display-format text))
                                  (text (substring-no-properties text))
                                  (text (string-trim text)))
                             (throw 'found text))))))))
      (add-to-list 'nano-agenda--anniversaries (cons org-date diary))))
  (cdr (assoc org-date nano-agenda--anniversaries)))

(defvar nano-agenda--entries nil
  "Cached list of entried (indexed by date as (month day year))")

(defun nano-agenda-entries (org-date &optional force-update)
  "Return entries for given DATE as (month day year) and force
update if FORCE-UPDATE is t.

Each entry is checked with nano-agenda-filter-predicate to decide
whether to include it or not. When included, each entry is then
checked for potential conflicts using nano-agenda-conflict-predicate.
Finally, entry are sorted using nano-agenda-sort-predicate."

;;  (when (or (not (assoc org-date nano-agenda--entries)) force-update)
    ;; Collect entries
    (let* ((entries nil))
      (dolist (file (org-agenda-files))
        (dolist (entry (org-agenda-get-day-entries file org-date :timestamp :scheduled :deadline))
          (if (funcall nano-agenda-filter-predicate entry)
              (add-to-list 'entries entry))))

      ;; Sort entries
      (sort entries nano-agenda-sort-predicate)

      ;; Check and mark conflicts
      (dolist (i (number-sequence 0 (1- (max 2 (length entries)))))
        (dolist (j (number-sequence (1+ i) (1- (length entries))))
          (let* ((entry-1 (nth i entries))
                 (entry-2 (nth j entries)))
            (when (funcall nano-agenda-conflict-predicate entry-1 entry-2)
              (add-text-properties 0 (length entry-1) '(conflict t) entry-1)
              (add-text-properties 0 (length entry-2) '(conflict t) entry-2)))))
      entries))

(defun nano-agenda--entry-time (entry)
  "Return the start and end time of ENTRY (if any)"

  (when-let* ((date (get-text-property 0 'date entry))
              (date (if (numberp date)
                        (calendar-gregorian-from-absolute date)
                      date))
              (time-of-day (get-text-property 0 'time-of-day entry))
              (duration (get-text-property 0 'duration entry))
              (month (nth 0 date))
              (day (nth 1 date))
              (year (nth 2 date))
              (hour (/ time-of-day 100))
              (minutes (- time-of-day (* hour 100)))
              (start (encode-time 0 minutes hour day month year))
              (end (encode-time 0 (+ minutes (floor duration)) hour day month year)))
    (cons start end)))

(defun nano-agenda--entry-daterange (entry)
  "Return (count . total) if ENTRY has a date range spanning several days"

  (let ((extra (get-text-property 0 'extra entry)))
    (save-match-data
      (when (and extra
                 (string-match "(\\([0-9]?\\)/\\([0-9]*\\)): " extra))
        (cons (string-to-number (match-string 1 extra))
              (string-to-number (match-string 2 extra)))))))

(defun nano-agenda--entry-header (entry)
  "Return the header text of ENTRY"

  (let* ((todo (get-text-property 0 'todo-state entry))
         (header (get-text-property 0 'txt entry))
         (header (org-link-display-format header))
         (header (replace-regexp-in-string org-tsr-regexp "" header))
         (header (replace-regexp-in-string "[ ]*:.*:$" "" header))
         (header (replace-regexp-in-string (or todo "") "" header))
         (header (string-trim header)))
    (substring-no-properties (or header ""))))

(defun nano-agenda--entry-tags (entry)
  "Return the first tag of ENTRY (if any)"

  (let* ((tags (mapcar (lambda (tag)
                         (let* ((tag (substring-no-properties tag))
                                (face (org-get-tag-face tag)))
                           (propertize tag 'face face)))
                       (get-text-property 0 'tags entry))))
    (mapconcat #'identity tags ",")))


(defun nano-agenda--entry-is-deadline (entry)
  "Return t if ENTRY is a deadline"

  (string= (get-text-property 0 'type entry) "deadline"))

(defun nano-agenda--entry-is-todo (entry)
  "Return t if ENTRY is a todo"

  (get-text-property 0 'todo-state entry))

(defun nano-agenda--entry-is-cancelled (entry)
  "Return t if ENTRY is cancelled"

  (string-search ":CANCELLED:" entry))

(defun nano-agenda--entry-is-conflict (entry)
  "Return t if ENTRY is conflicting with another entry"

  (get-text-property 0 'conflict entry))

(defun nano-agenda-forward-line (n)
  "Move N lines forward, add newlines if necessary"

  (let ((n (forward-line n)))
    (insert (make-string n ?\n))))


(defun nano-agenda--entry-format (entry)
  "Return a formatted org agenda entry in compact form."

  (let* ((time (nano-agenda--entry-time entry))
         (is-now (and (car time) (cdr time)
                      (time-less-p (car time) (current-time))
                      (time-less-p (current-time) (cdr time))))
         (is-todo (nano-agenda--entry-is-todo entry))
         (is-conflict (nano-agenda--entry-is-conflict entry))
         (is-deadline (nano-agenda--entry-is-deadline entry))
         (tags (nano-agenda--entry-tags entry))
         (daterange (nano-agenda--entry-daterange entry))
         (header (nano-agenda--entry-header entry))
         (header-face   'default)
         (time-face     (if is-conflict 'error 'nano-faded))
         (deadline-face 'error-i)
         (todo-face     'default-i)
         (tag-align (if (and tags nano-agenda-tags-align)
                        (propertize " " 'display `(space :align-to (- right 1 ,(length tags))))
                       " "))
         (header (concat (propertize header 'face header-face)))
         (prefix (cond (daterange
                        (propertize (format " %d/%d "
                                            (car daterange) (cdr daterange)
                                            'face time-face)))
                       (is-deadline
                         (propertize " TODO" 'face 'error))
                       (is-todo
                         (propertize " TODO" 'face 'font-lock-comment-face))
                       (time
                        (propertize
                               (format-time-string "%H:%M" (car time))
                               'face time-face))
                       (t (propertize  "—————" 'face time-face)))))

    (setq nano-agenda--entry-is-now (or nano-agenda--entry-is-now is-now))
    (concat " "
            prefix
            (propertize " │ " 'face 'nano-subtle-i)
            header
            tag-align
            tags)))

(defvar nano-agenda--date-occupancies nil
  "Cached list of (date occupancy deadline) for internal use")

(defun nano-agenda--date-occupancy (date &optional force-update)
  "Compute occupancy at a given date (in terms of agenda entries).
Occupancies are cached for efficiency."

  (let* ((date (decode-time date))
         (day   (nth 3 date))
         (month (nth 4 date))
         (year  (nth 5 date))
         (org-date  (list month day year))
         (occupancy 0)
         (deadline nil)
         (entry (assoc org-date nano-agenda--date-occupancies)))
    (if (and entry (not force-update))
        (progn
          (cadr entry))
      (progn
          (dolist (file (org-agenda-files))
            (dolist (entry (org-agenda-get-day-entries file org-date))
              (when (string-equal (get-text-property 0 'type entry) "deadline")
                (setq deadline t))
              (setq occupancy (+ occupancy
                     (funcall #'nano-agenda--entry-occupancy entry)))))
          (add-to-list 'nano-agenda--date-occupancies `(,org-date ,occupancy ,deadline))
          occupancy))))

(defun nano-agenda-date-equal (date-1 date-2)
  "Return t if DATE-1 is equal to DATE-2 (irrespective of time)"

  (let ((date-1 (decode-time date-1))
        (date-2 (decode-time date-2)))
    (and (eq (nth 3 date-1) (nth 3 date-2))
         (eq (nth 4 date-1) (nth 4 date-2))
         (eq (nth 5 date-1) (nth 5 date-2)))))

(defun nano-agenda-calendar--day (date displayed-month &optional force-update palette)
  (let* ((is-today (nano-agenda-date-equal date (current-time)))
         (is-selected (nano-agenda-date-equal date nano-agenda-date))
         (day (nth 3 (decode-time date)))
         (month (nth 4 (decode-time date)))
         (year (nth 5 (decode-time date)))
         (org-date (list month day year))
         (week-day (calendar-day-of-week (list month day year)))
         (week-day (mod (1- week-day) 7))
         (palette (or palette nano-agenda-palette))
         (palette (alist-get palette nano-agenda-palettes))
         (occupancy (nano-agenda--date-occupancy date force-update))
         (occupancy (min (length palette) occupancy))
         (is-holidays (nano-agenda-holidays org-date))
         (is-occupied (> occupancy 0))
         (is-offday (not (eq month displayed-month)))
         (is-weekend (memq week-day '(5 6)))
         (background-color (nth (- occupancy 1) palette))
         (foreground-color (if (< (nano-agenda-color-luminance background-color) 0.5)
                               "white"
                               "black"))
         (face (cond (is-offday     'nano-agenda-calendar-offday)
                     (is-selected   'nano-agenda-calendar-selected)
                     (is-holidays   'nano-agenda-calendar-holidays)
                     (is-occupied `(:background ,background-color
                                    :foreground ,foreground-color
                                    :inherit ,(when is-today 'nano-agenda-calendar-today)))
                     (is-today      'nano-agenda-calendar-today)
                     (is-weekend    'nano-agenda-calendar-weekend)
                     (t             'nano-agenda-calendar-default))))
    (insert (propertize (format-time-string "%e " date) 'face face))))

(defun nano-agenda--insert-calendar (date &optional force-update palette)

  (let* ((date (decode-time date))
         (day (nth 3 date))
         (month (nth 4 date))
         (year (nth 5 date))
         (date (encode-time (list 0 0 0 1 month year)))
         (weekday-names (mapcar #'(lambda (day)
                                (substring (calendar-day-name day t t) 0 2))
                            '(1 2 3 4 5)))
         (weekend-names (mapcar #'(lambda (day)
                                    (substring (calendar-day-name day t t) 0 2))
                                '(6 0)))
         (first-day (mod (1- (calendar-day-of-week (list month 1 year))) 7))
         (title (format-time-string "%B %Y" date))
         (title (let ((extra (max 0 (- 20 (length title)))))
                  (concat
                   (make-string (ceiling extra 2) ?\s)
                   title
                   (make-string (floor extra 2) ?\s)))))

    (insert (propertize title 'face 'nano-agenda-calendar-header-month))
    (insert "\n")

    (insert (propertize (mapconcat #'concat weekday-names " ")
                        'face '(nano-agenda-calendar-header-days
                                )))
    (insert " ")
    (insert (propertize (mapconcat #'concat weekend-names " ")
                        'face '(nano-agenda-calendar-weekend
                                nano-agenda-calendar-header-days)))
    (insert "\n")
    (dotimes (row 6)
      (dotimes (col 7)
        (let* ((day (- (+ col (* row 7)) first-day -1))
               (date (encode-time (list 0 0 0 day month year))))
          (nano-agenda-calendar--day date month force-update palette)))
      (insert "\n"))))


(defun nano-agenda--insert-agenda (date)
  "Insert agenda entries for DATE"

  (let* ((day (nth 3 (decode-time date)))
         (month (nth 4 (decode-time date)))
         (year (nth 5 (decode-time date)))
         (org-date (list month day year))
         (holidays (nano-agenda-holidays org-date))
         (anniversaries (nano-agenda-anniversaries org-date))
         (width (- (window-width) 24 3 9))
;;         (subtitle (or anniversaries
;;                       holidays
;;                       (format-time-string "Week %W" date)))
         (subtitle (or anniversaries holidays))
         (subtitle (when subtitle (truncate-string-to-width subtitle width nil nil "…")))
         (entries (nano-agenda-entries org-date))
         (separation "")
         (entry-marker  "")
         (padding (propertize " " 'display '(space :align-to (+ left 22))))
         (title (format-time-string "%A %d %B %Y" date))
         (week (format-time-string " (Week %V)" date)))

    (goto-char (line-end-position))
    (insert padding)
    (insert separation)
    (insert (propertize title 'face 'nano-agenda-header-title))
    (insert (propertize week 'face 'nano-agenda-header-subtitle))
    (nano-agenda-forward-line 1)
    (goto-char (line-end-position))
    (insert padding) (insert separation)
    (when subtitle
      (insert (propertize subtitle 'face 'nano-agenda-header-subtitle)))

    (when (nano-agenda-date-equal nano-agenda-date date)
      (setq nano-agenda--entry-index
            (if (> (length entries) 0)
                (mod nano-agenda--entry-index (length entries))
              -1))
      (setq nano-agenda--entry-marker nil))

    (dolist (i (number-sequence 0 (1- (length entries))))
      (let ((entry (nth i entries))
            (point nil)
            (highlight (and
                        (nano-agenda-date-equal nano-agenda-date date)
                        (eq i nano-agenda--entry-index))))
        (nano-agenda-forward-line 1)
        (goto-char (line-end-position))
        (setq point (point))
        (insert padding)
        (if highlight
          (progn
            (insert entry-marker)
            (setq nano-agenda--entry-marker
                  (get-text-property 0 'org-marker entry)))
          (progn
            (insert " ")
            (insert separation)))
        (insert (nano-agenda--entry-format entry))))))

(defun nano-agenda--date (date days months years)
   "Get date + DAYS day & MONTH months & YEARS years"

   (let* ((date (decode-time date))
          (day (nth 3 date))
          (month (nth 4 date))
          (year (nth 5 date)))
     (encode-time 0 0 0 (+ day days) (+ month months) (+ year years))))

(defun nano-agenda-goto-date (days months years)
   "Go to current date + DAYS day & MONTH months & YEARS years"

   (let* ((date (nano-agenda--date nano-agenda-date days months years)))
     (setq nano-agenda-date date)
     (setq nano-agenda--entry-index 0)
     (setq nano-agenda--entry-marker nil)
     (nano-agenda-update)))

(defun nano-agenda-hide-entry ()
  "Hide current entry (if any)"

  (interactive)
  (when nano-agenda--entry-window
    (delete-window nano-agenda--entry-window))
  (setq nano-agenda--entry-window nil))

(defun nano-agenda-view-entry ()
  "Edit current entry (if any)"

  (interactive)
  (when nano-agenda--entry-marker
    (let* ((window (get-buffer-window))
           (buttons '(("CLOSE" . (nano-agenda-hide-entry)))))
      (nano-agenda-edit-entry)
      (read-only-mode 1)
      (select-window window)
      (goto-char (point-min)))))

(defun nano-agenda-edit-entry ()
  "Edit current entry (if any)"

  (interactive)
  (when-let* ((marker nano-agenda--entry-marker)
              (buffer (marker-buffer marker))
              (position (marker-position marker))
              (org-indirect-buffer-display 'current-window))
    (if (window-live-p nano-agenda--entry-window)
        (progn
          (select-window nano-agenda--entry-window)
          (switch-to-buffer buffer))
      (progn
        (pop-to-buffer buffer '((display-buffer-at-bottom)))))
    (widen)
    (goto-char position)
    (org-tree-to-indirect-buffer)
    (fit-window-to-buffer nil 12 8)
    (let* ((components (org-heading-components))
           (heading (nth 4 components))
           (heading (replace-regexp-in-string
                     org-link-bracket-re "\\2" heading))
           (heading (replace-regexp-in-string
                     org-element--timestamp-regexp "" heading))
           (heading (string-trim heading)))
    (rename-buffer heading))
    (setq nano-agenda--entry-window (get-buffer-window nil t))
    (read-only-mode -1)
    (when (functionp 'nano-box-on)
      (nano-box-on))))

(defun nano-agenda-goto-today ()
   "Go to previous day"

   (interactive)
   (setq nano-agenda-date (current-time))
   (nano-agenda-goto-date 0 0 0))

(defun nano-agenda-goto-next-entry ()
   "Go to next day"

   (interactive)
   (when (numberp nano-agenda--entry-index)
     (setq nano-agenda--entry-index
           (1+ nano-agenda--entry-index))
     (nano-agenda-update)))

(defun nano-agenda-goto-prev-entry ()
   "Go to next day"

   (interactive)
   (when (numberp nano-agenda--entry-index )
     (setq nano-agenda--entry-index
           (1- nano-agenda--entry-index))
     (nano-agenda-update)))

(defun nano-agenda-goto-prev-day ()
   "Go to previous day"

   (interactive)
   (nano-agenda-goto-date -1 0 0))

 (defun nano-agenda-goto-next-day ()
   "Go to next day"

   (interactive)
   (nano-agenda-goto-date +1 0 0))

(defun nano-agenda-goto-prev-week ()
   "Go to previous week"

   (interactive)
   (nano-agenda-goto-date -7 0 0))

 (defun nano-agenda-goto-next-week ()
   "Go to next week"

   (interactive)
   (nano-agenda-goto-date +7 0 0))

(defun nano-agenda-goto-prev-month ()
   "Go to previous month"

   (interactive)
   (nano-agenda-goto-date 0 -1 0))

 (defun nano-agenda-goto-next-month ()
   "Go to next month"

   (interactive)
   (nano-agenda-goto-date 0 +1 0))

(defun nano-agenda-view-mode-day ()
  "Set agenda view mode to day"

  (interactive)
  (setq nano-agenda-view-mode 'day)
  (nano-agenda-update)
  (fit-window-to-buffer)
  (window-resize nil 1))

(defun nano-agenda-view-mode-week ()
  "Set agenda view mode to week"

  (interactive)
  (setq nano-agenda-view-mode 'week)
  (nano-agenda-update)
  (fit-window-to-buffer)
  (window-resize nil 1))

(defun nano-agenda-update ()
  "Update agenda"

  (interactive)
  (with-current-buffer (get-buffer-create nano-agenda-buffer-name)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (setq nano-agenda--entry-is-now nil)
      (let* ((date (decode-time nano-agenda-date))
             (day (nth 3 date))
             (month (nth 4 date))
             (year (nth 5 date))
             (prev-month (encode-time (list 0 0 0 1 (1- month) year)))
             (curr-month (encode-time (list 0 0 0 1 month year)))
             (next-month (encode-time (list 0 0 0 1 (1+ month) year))))
        (when (eq nano-agenda-view-mode 'week)
          (nano-agenda--insert-calendar prev-month)
          (insert "\n"))
        (nano-agenda--insert-calendar curr-month)
        (when (eq nano-agenda-view-mode 'week)
          (insert "\n")
          (nano-agenda--insert-calendar next-month)))
      (goto-char (point-min))

      (let* ((date (decode-time nano-agenda-date))
             (day (nth 3 date))
             (month (nth 4 date))
             (year (nth 5 date))
             (day-of-week (mod (1- (calendar-day-of-week (list month day year))) 7))
             (first-day-of-week (encode-time (list 0 0 0 (- day day-of-week) month year))))
        (if (eq nano-agenda-view-mode 'week)
            (dotimes (inc 7)
              (nano-agenda--insert-agenda
               (nano-agenda--date first-day-of-week inc 0 0))
              (nano-agenda-forward-line 2))
          (nano-agenda--insert-agenda nano-agenda-date)))
      (when nano-agenda-header-show
        (goto-char (point-min))
        (insert "\n")
        (nano-agenda-header))
      (force-mode-line-update)
      (run-hooks 'nano-agenda-update-hook))))

(defun nano-agenda-force-update ()
  "Update agenda"

  (interactive)
  (setq nano-agenda--date-occupancies nil)
  (setq nano-agenda--entries nil)
  (setq nano-agenda--holidays nil)
  (setq nano-agenda--anniversaries nil)
  (nano-agenda-update))

(defun nano-agenda ()
  "Insert an agenda in the agenda buffer and install a time for
regular update."

  (interactive)
  (switch-to-buffer (get-buffer-create nano-agenda-buffer-name))
  ;; (set-window-dedicated-p nil t)
  (nano-agenda-update)
  (when (fboundp 'show-paren-local-mode)
    (show-paren-local-mode 0))
  (nano-agenda-mode 1)
  nano-agenda-buffer-name)

(define-minor-mode nano-agenda-mode
  "Minor mode for nano-agenda day view."
  :init nil
  :keymap `((,(kbd "n")         . nano-agenda-goto-next-entry)
            (,(kbd "<tab>")     . nano-agenda-goto-next-entry)
            (,(kbd "p")         . nano-agenda-goto-prev-entry)
            (,(kbd "<SPC>")     . nano-agenda-view-entry)
            (,(kbd "<return>")  . nano-agenda-edit-entry)
            (,(kbd "c")         . org-capture)
            (,(kbd "r")         . nano-agenda-force-update)
            (,(kbd "d")         . nano-agenda-view-mode-day)
            (,(kbd "w")         . nano-agenda-view-mode-week)
            (,(kbd "g")         . nano-agenda-update)
            (,(kbd "q")         . kill-current-buffer)
            (,(kbd "<left>")    . nano-agenda-goto-prev-day)
            (,(kbd "<right>")   . nano-agenda-goto-next-day)
            (,(kbd "<up>")      . nano-agenda-goto-prev-week)
            (,(kbd "<down>")    . nano-agenda-goto-next-week)
            (,(kbd "<S-left>")  . nano-agenda-goto-prev-month)
            (,(kbd "<S-right>") . nano-agenda-goto-next-month)
            (,(kbd ".")         . nano-agenda-goto-today))
  (when nano-agenda-mode
    (setq-local cursor-type nil)
    (setq buffer-read-only t)))

(provide 'nano-agenda)
;;; nano-agenda.el ends here
