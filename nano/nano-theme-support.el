;;; nano-theme-support.el --- N Λ N O theme -*- lexical-binding: t -*-

;; Copyright (C) 2021,2022 Free Software Foundation, Inc.

;; Maintainer: Nicolas P. Rougier <Nicolas.Rougier@inria.fr>
;; URL: https://github.com/rougier/nano-theme
;; Version: 0.3.4
;; Package-Requires: ((emacs "27.1"))
;; Keywords: theme, dark, light

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
;; N Λ N O theme is a consistent theme that comes in two flavors:
;;  - a light theme that is based on Material (https://material.io/)
;;  - a dark theme that is based on Nord (https://www.nordtheme.com/).
;;
;; A theme is fully defined by a set of (1+6) faces as
;; explained in this paper https://arxiv.org/abs/2008.06030:
;;
;; - Default face is the face for regular information.
;;
;; - Critical face is for information that requires immediate action.
;;
;;     It should be of high constrast when compared to other
;;     faces. This can be realized (for example) by setting an intense
;;     background color, typically a shade of red. It must be used
;;     scarcely.
;;
;; - Popout face is used for information that needs attention.
;;
;;     To achieve such effect, the hue of the face has to be
;;     sufficiently different from other faces such that it attracts
;;     attention through the popout effect.
;;
;; - Strong face is used for information of a structural nature.
;;
;;     It has to be the same color as the default color and only the
;;     weight differs by one level (e.g., light/regular or
;;     regular/bold). IT is generally used for titles, keywords,
;;     directory, etc.
;;
;; - Salient face is used for information that are important.
;;
;;     To suggest the information is of the same nature but important,
;;     the face uses a different hue with approximately the same
;;     intensity as the default face. This is typically used for
;;     links.

;; - Faded face is for information that are less important.
;;
;;     It is made by using the same hue as the default but with a
;;     lesser intensity than the default. It can be used for comments,
;;     secondary information and also replace italic (which is
;;     generally abused anyway
;;
;; - Subtle face is used to suggest a physical area on the screen.
;;
;;     It is important to not disturb too strongly the reading of
;;     information and this can be made by setting a very light
;;     background color that is barely perceptible.
;;

;; Usage example:
;;
;; You can use the theme as a regular theme or you can call
;; (nano-light) / (nano-dark) explicitely to install the light or dark
;; version.
;;
;; With GUI, you can mix frames with light and dark mode. Just call
;; (nano-new-frame 'light) or (nano-new-frame 'dark)
;;
;; Optionally, you can use (nano-mode) to setup recommended settings for
;; the theme. Be careful since it will modify your configuration and
;; requires a set of specific fonts. This needs to be called before
;; setting the theme
;;
;; Recommended font is "Roboto Mono" or "Roboto Mono Nerd" if you want
;; to benefit from all the fancy glyphs. See https://www.nerdfonts.com.

;;; NEWS:

;; Version 0.3.5
;; - Added diff-hl faces
;; - Modified line-num er faces

;; Version 0.3.3
;; - Removed debug message
;; - Minor changes in agenda

;; Version 0.3.2
;; - Fix magit diff whitespace
;; - Update mu4e faces (1.8.x release)
;; - Added rounded corners for emacs-plus@29

;; Version 0.3.1
;; - Modified vertico and org modes
;; - Added imenu-list, ansi-color and SHR faces

;; Version 0.3.0
;; - Added italic (Victor Mono)
;; - Less salient critical face
;; - Added orderles, marginalia & corfu faces

;; Version 0.2.1
;; - Added nano-modeline faces

;; Version 0.2
;; - Split light / dark themes properly
;; - Added a nano-new-frame function
;;
;; Version 0.1
;; - Submission to ELPA

;;; Code:
(require 'disp-table)
(require 'cl-lib)

(defgroup nano nil
  "N Λ N O"
  :group 'convenience)

(defgroup nano-theme nil
  "N Λ N O Theme"
  :group 'nano)

(defgroup nano-theme-light nil
  "Light color palette"
  :group 'nano-theme)

(defgroup nano-theme-dark nil
  "Dark color palette"
  :group 'nano-theme)

(defgroup nano-theme-fonts nil
  "Font stack"
  :group 'nano-theme)

(defcustom nano-fonts-use nil
  "whether to use font stack"
  :type 'boolean :group 'nano-theme-fonts)

(defcustom nano-window-divider-show nil
  "Whether to show the vertical window-divider"
  :type 'boolean :group 'nano-theme)

(defface nano-mono
  '((t (:family "Iosevka" :height 110 :weight light)))
  "Default monospaced font ."
  :group 'nano-theme-fonts)

(defface nano-sans
  '((t (:family "Inter" :height 110 :weight light)))
  "Default proportional sans font."
  :group 'nano-theme-fonts)

;; Colours mooched off of Catppuccin Latte and Frappe

(defcustom nano-light-rosewater "#dc8a78"
  "Light theme colour: rosewater."
  :type 'color :group 'nano-theme-light)
(defcustom nano-light-flamingo "#dd7878"
  "Light theme colour: flamingo."
  :type 'color :group 'nano-theme-light)
(defcustom nano-light-pink "#ea76cb"
  "Light theme colour: pink."
  :type 'color :group 'nano-theme-light)
(defcustom nano-light-mauve "#8839ef"
  "Light theme colour: mauve."
  :type 'color :group 'nano-theme-light)
(defcustom nano-light-red "#d20f39"
  "Light theme colour: red."
  :type 'color :group 'nano-theme-light)
(defcustom nano-light-maroon "#e64553"
  "Light theme colour: maroon."
  :type 'color :group 'nano-theme-light)
(defcustom nano-light-peach "#fe640b"
  "Light theme colour: peach."
  :type 'color :group 'nano-theme-light)
(defcustom nano-light-yellow "#df8e1d"
  "Light theme colour: yellow."
  :type 'color :group 'nano-theme-light)
(defcustom nano-light-green "#40a02b"
  "Light theme colour: green."
  :type 'color :group 'nano-theme-light)
(defcustom nano-light-teal "#179299"
  "Light theme colour: teal."
  :type 'color :group 'nano-theme-light)
(defcustom nano-light-sky "#04a5e5"
  "Light theme colour: sky."
  :type 'color :group 'nano-theme-light)
(defcustom nano-light-sapphire "#209fb5"
  "Light theme colour: sapphire."
  :type 'color :group 'nano-theme-light)
(defcustom nano-light-blue "#1e66f5"
  "Light theme colour: blue."
  :type 'color :group 'nano-theme-light)
(defcustom nano-light-lavender "#7287fd"
  "Light theme colour: lavender."
  :type 'color :group 'nano-theme-light)
(defcustom nano-light-text "#4c4f69"
  "Light theme colour: text."
  :type 'color :group 'nano-theme-light)
(defcustom nano-light-subtext1 "#5c5f77"
  "Light theme colour: subtext1."
  :type 'color :group 'nano-theme-light)
(defcustom nano-light-subtext0 "#6c6f85"
  "Light theme colour: subtext0."
  :type 'color :group 'nano-theme-light)
(defcustom nano-light-overlay2 "#7c7f93"
  "Light theme colour: overlay2."
  :type 'color :group 'nano-theme-light)
(defcustom nano-light-overlay1 "#8c8fa1"
  "Light theme colour: overlay1."
  :type 'color :group 'nano-theme-light)
(defcustom nano-light-overlay0 "#9ca0b0"
  "Light theme colour: overlay0."
  :type 'color :group 'nano-theme-light)
(defcustom nano-light-surface2 "#acb0be"
  "Light theme colour: surface2."
  :type 'color :group 'nano-theme-light)
(defcustom nano-light-surface1 "#bcc0cc"
  "Light theme colour: surface1."
  :type 'color :group 'nano-theme-light)
(defcustom nano-light-surface0 "#ccd0da"
  "Light theme colour: surface0."
  :type 'color :group 'nano-theme-light)
(defcustom nano-light-base "#eff1f5"
  "Light theme colour: base."
  :type 'color :group 'nano-theme-light)
(defcustom nano-light-mantle "#e6e9ef"
  "Light theme colour: mantle."
  :type 'color :group 'nano-theme-light)
(defcustom nano-light-crust "#dce0e8"
  "Light theme colour: crust."
  :type 'color :group 'nano-theme-light)

(defcustom nano-dark-rosewater "#f2d5cf"
  "Dark theme colour: rosewater."
  :type 'color :group 'nano-theme-dark)
(defcustom nano-dark-flamingo "#eebebe"
  "Dark theme colour: flamingo."
  :type 'color :group 'nano-theme-dark)
(defcustom nano-dark-pink "#f4b8e4"
  "Dark theme colour: pink."
  :type 'color :group 'nano-theme-dark)
(defcustom nano-dark-mauve "#ca9ee6"
  "Dark theme colour: mauve."
  :type 'color :group 'nano-theme-dark)
(defcustom nano-dark-red "#e78284"
  "Dark theme colour: red."
  :type 'color :group 'nano-theme)
(defcustom nano-dark-maroon "#ea999c"
  "Dark theme colour: maroon."
  :type 'color :group 'nano-theme-dark)
(defcustom nano-dark-peach "#ef9f76"
  "Dark theme colour: peach."
  :type 'color :group 'nano-theme-dark)
(defcustom nano-dark-yellow "#e5c890"
  "Dark theme colour: yellow."
  :type 'color :group 'nano-theme-dark)
(defcustom nano-dark-green "#a6d189"
  "Dark theme colour: green."
  :type 'color :group 'nano-theme-dark)
(defcustom nano-dark-teal "#81c8be"
  "Dark theme colour: teal."
  :type 'color :group 'nano-theme-dark)
(defcustom nano-dark-sky "#99d1db"
  "Dark theme colour: sky."
  :type 'color :group 'nano-theme-dark)
(defcustom nano-dark-sapphire "#85c1dc"
  "Dark theme colour: sapphire."
  :type 'color :group 'nano-theme-dark)
(defcustom nano-dark-blue "#8caaee"
  "Dark theme colour: blue."
  :type 'color :group 'nano-theme-dark)
(defcustom nano-dark-lavender "#babbf1"
  "Dark theme colour: lavender."
  :type 'color :group 'nano-theme-dark)
(defcustom nano-dark-text "#c6d0f5"
  "Dark theme colour: text."
  :type 'color :group 'nano-theme-dark)
(defcustom nano-dark-subtext1 "#b5bfe2"
  "Dark theme colour: subtext1."
  :type 'color :group 'nano-theme-dark)
(defcustom nano-dark-subtext0 "#a5adce"
  "Dark theme colour: subtext0."
  :type 'color :group 'nano-theme-dark)
(defcustom nano-dark-overlay2 "#949cbb"
  "Dark theme colour: overlay2."
  :type 'color :group 'nano-theme-dark)
(defcustom nano-dark-overlay1 "#838ba7"
  "Dark theme colour: overlay1."
  :type 'color :group 'nano-theme-dark)
(defcustom nano-dark-overlay0 "#737994"
  "Dark theme colour: overlay0."
  :type 'color :group 'nano-theme-dark)
(defcustom nano-dark-surface2 "#626880"
  "Dark theme colour: surface2."
  :type 'color :group 'nano-theme-dark)
(defcustom nano-dark-surface1 "#51576d"
  "Dark theme colour: surface1."
  :type 'color :group 'nano-theme-dark)
(defcustom nano-dark-surface0 "#414559"
  "Dark theme colour: surface0."
  :type 'color :group 'nano-theme-dark)
(defcustom nano-dark-base "#303446"
  "Dark theme colour: base."
  :type 'color :group 'nano-theme-dark)
(defcustom nano-dark-mantle "#292c3c"
  "Dark theme colour: mantle."
  :type 'color :group 'nano-theme-dark)
(defcustom nano-dark-crust "#232634"
  "Dark theme colour: crust."
  :type 'color :group 'nano-theme-dark)

(defcustom nano-rosewater nil
  "Theme colour: rosewater."
  :type 'color :group 'nano-theme)
(defcustom nano-flamingo nil
  "Theme colour: flamingo."
  :type 'color :group 'nano-theme)
(defcustom nano-pink nil
  "Theme colour: pink."
  :type 'color :group 'nano-theme)
(defcustom nano-mauve nil
  "Theme colour: mauve."
  :type 'color :group 'nano-theme)
(defcustom nano-red nil
  "Theme colour: red."
  :type 'color :group 'nano-theme)
(defcustom nano-maroon nil
  "Theme colour: maroon."
  :type 'color :group 'nano-theme)
(defcustom nano-peach nil
  "Theme colour: peach."
  :type 'color :group 'nano-theme)
(defcustom nano-yellow nil
  "Theme colour: yellow."
  :type 'color :group 'nano-theme)
(defcustom nano-green nil
  "Theme colour: green."
  :type 'color :group 'nano-theme)
(defcustom nano-teal nil
  "Theme colour: teal."
  :type 'color :group 'nano-theme)
(defcustom nano-sky nil
  "Theme colour: sky."
  :type 'color :group 'nano-theme)
(defcustom nano-sapphire nil
  "Theme colour: sapphire."
  :type 'color :group 'nano-theme)
(defcustom nano-blue nil
  "Theme colour: blue."
  :type 'color :group 'nano-theme)
(defcustom nano-lavender nil
  "Theme colour: lavender."
  :type 'color :group 'nano-theme)
(defcustom nano-text nil
  "Theme colour: text."
  :type 'color :group 'nano-theme)
(defcustom nano-subtext1 nil
  "Theme colour: subtext1."
  :type 'color :group 'nano-theme)
(defcustom nano-subtext0 nil
  "Theme colour: subtext0."
  :type 'color :group 'nano-theme)
(defcustom nano-overlay2 nil
  "Theme colour: overlay2."
  :type 'color :group 'nano-theme)
(defcustom nano-overlay1 nil
  "Theme colour: overlay1."
  :type 'color :group 'nano-theme)
(defcustom nano-overlay0 nil
  "Theme colour: overlay0."
  :type 'color :group 'nano-theme)
(defcustom nano-surface2 nil
  "Theme colour: surface2."
  :type 'color :group 'nano-theme)
(defcustom nano-surface1 nil
  "Theme colour: surface1."
  :type 'color :group 'nano-theme)
(defcustom nano-surface0 nil
  "Theme colour: surface0."
  :type 'color :group 'nano-theme)
(defcustom nano-base nil
  "Theme colour: base."
  :type 'color :group 'nano-theme)
(defcustom nano-mantle nil
  "Theme colour: mantle."
  :type 'color :group 'nano-theme)
(defcustom nano-crust nil
  "Theme colour: crust."
  :type 'color :group 'nano-theme)

(defface nano-critical nil
  "Critical face is for information that requires immediate action.
It should be of high constrast when compared to other faces. This
can be realized (for example) by setting an intense background
color, typically a shade of red. It must be used scarcely."
  :group nil)

(defface nano-critical-i nil
  "Critical face inversed."
  :group nil)

(defface nano-popout nil
  "Popout face is used for information that needs attention.
To achieve such effect, the hue of the face has to be
sufficiently different from other faces such that it attracts
attention through the popout effect."
  :group nil)

(defface nano-popout-i nil
  "Popout face inversed."
  :group nil)

(defface nano-strong nil
  "Strong face is used for information of a structural nature.
It has to be the same color as the default color and only the
weight differs by one level (e.g., light/regular or
regular/bold). IT is generally used for titles, keywords,
directory, etc."
  :group nil)

(defface nano-strong-i nil
  "Strong face inversed."
  :group nil)

(defface nano-salient nil
  "Salient face is used for information that are important.
To suggest the information is of the same nature but important,
the face uses a different hue with approximately the same
intensity as the default face. This is typically used for links."
  :group nil)

(defface nano-salient-i nil
  "Strong face inversed."
  :group nil)

(defface nano-faded nil
  "Faded face is for information that are less important.
It is made by using the same hue as the default but with a lesser
intensity than the default. It can be used for comments,
secondary information and also replace italic (which is generally
abused anyway)."
  :group nil)

(defface nano-faded-i nil
  "Faded face inversed."
  :group nil)

(defface nano-subtle nil
  "Subtle face is used to suggest a physical area on the screen.
It is important to not disturb too strongly the reading of
information and this can be made by setting a very light
background color that is barely perceptible."
  :group nil)

(defface nano-subtle-i nil
  "Subtle face inversed."
  :group nil)

(defface nano-default nil
  "Default face."
  :group nil)

(defface nano-default-i nil
  "Default face inversed."
  :group nil)

(defun nano-mode ()
  "Defaults settings for nano (optional)"
  (interactive)

  ;; Use nano fonts
  (setq nano-fonts-use t)

  ;; No startup screen
  (setq inhibit-startup-screen t)

  ;; No startup message
  (setq inhibit-startup-message t)
  (setq inhibit-startup-echo-area-message t)

  ;; No message in scratch buffer
  (setq initial-scratch-message nil)

  ;; Initial buffer
  (setq initial-buffer-choice nil)

  ;; No frame title
  (setq frame-title-format nil)

  ;; No file dialog
  (setq use-file-dialog nil)

  ;; No dialog box
  (setq use-dialog-box nil)

  ;; No popup windows
  (setq pop-up-windows nil)

  ;; No empty line indicators
  (setq indicate-empty-lines nil)

  ;; No cursor in inactive windows
  (setq cursor-in-non-selected-windows nil)

  ;; Text mode is initial mode
  (setq initial-major-mode 'text-mode)

  ;; Text mode is default major mode
  (setq default-major-mode 'text-mode)

  ;; Moderate font lock
  (setq font-lock-maximum-decoration t)

  ;; No line break space points
  (setq auto-fill-mode nil)

  ;; Fill column at 80
  (setq fill-column 80)

  ;; Bar cursor
  (setq-default cursor-type '(hbar .  2))
  (setq-default cursor-in-non-selected-windows nil)
  (setq blink-cursor-mode nil)

  ;; Default frame settings
  (setq default-frame-alist
        (append (list
                 '(min-height . 1)  '(height . 45)
                 '(min-width  . 1)  '(width  . 81)
                 '(vertical-scroll-bars . nil)
                 '(internal-border-width . 24)
                 '(left-fringe . 16)
                 '(right-fringe . 0)
		 '(line-spacing . 0.1)
                 '(undecorated-round . t) ;; emacs-plu@29 only
                 '(scroll-bar-mode . -1)
                 '(tool-bar-lines . 0)
		 '(tool-bar-position . left)
                 '(menu-bar-lines . 0))))

  (setq tool-bar-style 'image)
  (setq line-spacing 0.1)

  ;; Vertical window divider
  (setq window-divider-default-right-width 24)
  (setq window-divider-default-places 'right-only)
  (window-divider-mode 1)

  ;; No backup files
  (setq make-backup-files nil)

  ;; Nicer glyphs for continuation and wrap
  (set-display-table-slot standard-display-table
                          'truncation (make-glyph-code ?… 'nano-faded))
  (set-display-table-slot standard-display-table
                          'wrap (make-glyph-code ?- 'nano-faded))

  ;; Nerd font for glyph icons
  (set-fontset-font t '(#x23fb . #x23fe) "Symbols Nerd Font")
  (set-fontset-font t '(#x2500 . #x259f) "Symbols Nerd Font")
  (set-fontset-font t '(#x276c . #x2771) "Symbols Nerd Font")
  (set-fontset-font t '(#xe000 . #xe00a) "Symbols Nerd Font")
  (set-fontset-font t '(#xe0a0 . #xe0a3) "Symbols Nerd Font")
  (set-fontset-font t '(#xe0b0 . #xe0d7) "Symbols Nerd Font")
  (set-fontset-font t '(#xe200 . #xe2a9) "Symbols Nerd Font")
  (set-fontset-font t '(#xe300 . #xe3e3) "Symbols Nerd Font")
  (set-fontset-font t '(#xe5fa . #xe6b7) "Symbols Nerd Font")
  (set-fontset-font t '(#xe700 . #xe8ef) "Symbols Nerd Font")
  (set-fontset-font t '(#xea60 . #xec1e) "Symbols Nerd Font")
  (set-fontset-font t '(#xed00 . #xf2ff) "Symbols Nerd Font")
  (set-fontset-font t '(#xee00 . #xee0b) "Symbols Nerd Font")
  (set-fontset-font t '(#xf300 . #xf381) "Symbols Nerd Font")
  (set-fontset-font t '(#xf400 . #xf533) "Symbols Nerd Font")
  (set-fontset-font t '(#xf0001 . #xf1af0) "Symbols Nerd Font")
  )


(defun nano-new-frame (&optional mode)
  "Create a new frame in light or dark MODE."

  (interactive)
  (let ((mode (or mode (frame-parameter nil 'background-mode)))
        (background-mode frame-background-mode)
        (selected-frame (selected-frame))
        (nano-theme-frame-only (make-frame-command)))
    (nano-theme nil mode)))


(defun nano-frame-list-advice-selected (_frames)
  (list (selected-frame)))

(defun nano-frame-list-advice-normal (frames)
  "Filter out FRAMES that are not standalone nano frames."
  (seq-filter (lambda (f) (not (frame-parameter f 'nano-theme-standalone))) frames))

(defun nano-frame-enable (mode &optional frame)
  "Enable nano MODE for the current frame or FRAME only."
  (let ((frame (window-frame))
        (frame-background-mode mode))
    (set-frame-parameter frame 'nano-theme-standalone mode)
    (frame-set-background-mode frame)
    (advice-add 'frame-list :filter-return #'nano-frame-list-advice-selected)
    (enable-theme 'nano)
    (advice-remove 'frame-list #'nano-frame-list-advice-selected)))

(defun nano-frame-dark (&optional frame)
  "Load the nano dark theme on FRAME or current frame."
  (interactive)
  (if (framep frame)
      (nano-frame-enable 'dark frame)
    (nano-frame-enable 'dark)))

(defun nano-frame-light (&optional frame)
  "Load the nano light theme on FRAME or current frame."
  (interactive)
  (if (framep frame)
      (nano-frame-enable 'light frame)
    (nano-frame-enable 'light)))

(defun nano-theme-frame-toggle ()
  "Toggle theme on current frame only."
  (interactive)
  (if (eq (or (frame-parameter (selected-frame) 'nano-theme-standalone) frame-background-mode) 'light)
      (nano-frame-dark)
    (nano-frame-light)))

(defun nano-enable (mode)
  "Enable nano MODE all other frames"
  (advice-add 'frame-list :filter-return #'nano-frame-list-advice-normal)
  (nano-theme 'nano mode)
  (enable-theme 'nano)
  (advice-remove 'frame-list #'nano-frame-list-advice-normal))

(defun nano-dark ()
  "Load the nano dark theme on current frame."
  (interactive)
  (nano-enable 'dark))

(defun nano-light ()
  "Load the nano light theme on current frame."
  (interactive)
  (nano-enable 'light))

(defun nano-theme-toggle ()
  "Toggle theme on all frames."
  (interactive)
  (if (eq frame-background-mode 'light)
      (nano-dark)
    (nano-light)))

(defun nano-theme (theme mode)
  "Apply the nano THEME according to MODE which can be 'dark or 'light."

  (message (format "N Λ N O Theme applied: %s" mode))

  (advice-add 'frame-list :filter-return #'nano-frame-list-advice-normal)

  (let ((light '((background light)))
        (dark  '((background dark))))

    (setq default-frame-alist
          (assq-delete-all 'foreground-color
                           (assq-delete-all 'background-color
                                            (assq-delete-all 'background-mode default-frame-alist))))
    (add-to-list 'default-frame-alist `(background-mode . ,mode))
    (add-to-list 'default-frame-alist `(background-color . ,(if (eq mode 'light)
                                                                nano-light-base
                                                              nano-dark-base)))
    (add-to-list 'default-frame-alist `(foreground-color . ,(if (eq mode 'light)
                                                                nano-light-text
                                                              nano-dark-text)))
    (custom-theme-set-variables theme '(widget-image-enable nil)
                                '(x-underline-at-descent-line t))
    (setq frame-background-mode mode)
    (mapc #'frame-set-background-mode (frame-list))

    ;; Set the colours
    (setq nano-rosewater
	  (if (eq mode 'light) nano-light-rosewater nano-dark-rosewater))
    (setq nano-flamingo
	  (if (eq mode 'light) nano-light-flamingo nano-dark-flamingo))
    (setq nano-pink
	  (if (eq mode 'light) nano-light-pink nano-dark-pink))
    (setq nano-mauve
	  (if (eq mode 'light) nano-light-mauve nano-dark-mauve))
    (setq nano-red
	  (if (eq mode 'light) nano-light-red nano-dark-red))
    (setq nano-maroon
	  (if (eq mode 'light) nano-light-maroon nano-dark-maroon))
    (setq nano-peach
	  (if (eq mode 'light) nano-light-peach nano-dark-peach))
    (setq nano-yellow
	  (if (eq mode 'light) nano-light-yellow nano-dark-yellow))
    (setq nano-green
	  (if (eq mode 'light) nano-light-green nano-dark-green))
    (setq nano-teal
	  (if (eq mode 'light) nano-light-teal nano-dark-teal))
    (setq nano-sky
	  (if (eq mode 'light) nano-light-sky nano-dark-sky))
    (setq nano-sapphire
	  (if (eq mode 'light) nano-light-sapphire nano-dark-sapphire))
    (setq nano-blue
	  (if (eq mode 'light) nano-light-blue nano-dark-blue))
    (setq nano-lavender
	  (if (eq mode 'light) nano-light-lavender nano-dark-lavender))
    (setq nano-text
	  (if (eq mode 'light) nano-light-text nano-dark-text))
    (setq nano-subtext1
	  (if (eq mode 'light) nano-light-subtext1 nano-dark-subtext1))
    (setq nano-subtext0
	  (if (eq mode 'light) nano-light-subtext0 nano-dark-subtext0))
    (setq nano-overlay2
	  (if (eq mode 'light) nano-light-overlay2 nano-dark-overlay2))
    (setq nano-overlay1
	  (if (eq mode 'light) nano-light-overlay1 nano-dark-overlay1))
    (setq nano-overlay0
	  (if (eq mode 'light) nano-light-overlay0 nano-dark-overlay0))
    (setq nano-surface2
	  (if (eq mode 'light) nano-light-surface2 nano-dark-surface2))
    (setq nano-surface1
	  (if (eq mode 'light) nano-light-surface1 nano-dark-surface1))
    (setq nano-surface0
	  (if (eq mode 'light) nano-light-surface0 nano-dark-surface0))
    (setq nano-base
	  (if (eq mode 'light) nano-light-base nano-dark-base))
    (setq nano-mantle
	  (if (eq mode 'light) nano-light-mantle nano-dark-mantle))
    (setq nano-crust
	  (if (eq mode 'light) nano-light-crust nano-dark-crust))

    (when nano-fonts-use
        (custom-theme-set-faces theme
         `(default ((t (:foreground ,nano-text
			:background ,nano-base
                        :weight     ,(face-attribute 'nano-mono :weight)
                        :height     ,(face-attribute 'nano-mono :height)
                        :family     ,(face-attribute 'nano-mono :family)))))
         `(italic ((t  (:foreground ,nano-text
			:background ,nano-base
                        :weight     ,(face-attribute 'nano-mono :weight)
                        :height     ,(face-attribute 'nano-mono :height)
                        :slant      italic))))
         `(nano-strong ((t (:weight medium ))))
         `(variable-pitch  ((t (:weight ,(face-attribute 'nano-sans :weight)
                                :height ,(face-attribute 'nano-sans :height)
                                :family ,(face-attribute 'nano-sans :family)))))))

    (unless nano-fonts-use
        (custom-theme-set-faces theme
         `(default ((t (:foreground ,nano-text))))
         `(nano-strong ((t (:weight medium :foreground ,nano-text))))))


    ;; --- Window divider ----------------------------------------------
    (if nano-window-divider-show
        (custom-theme-set-faces theme
         `(window-divider ((t (:foreground ,nano-text))))
         `(vertical-border ((t (:foreground ,nano-text)))))
      (custom-theme-set-faces theme
       `(window-divider ((t (:foreground ,nano-base))))
       `(vertical-border ((t (:foreground ,nano-base))))))
    (custom-theme-set-faces theme
     '(window-divider-first-pixel ((t (:inherit window-divider))))
     '(window-divider-last-pixel ((t (:inherit window-divider)))))


    (custom-theme-set-faces theme

   ;; --- Base ---------------------------------------------------------

   `(default ((t  (:background ,nano-base
                        :foreground ,nano-text))))

   `(cursor ((t (:foreground ,nano-base
                      :background ,nano-text))))

   `(mouse ((t (:foreground ,nano-text
                     :background ,nano-base))))

   `(highlight ((t (:background ,nano-mantle))))

   `(nano-subtle ((t (:background ,nano-crust))))

   `(nano-subtle-i ((t (:foreground ,nano-crust))))

   `(nano-faded ((t  (:foreground ,nano-overlay0))))

   `(nano-faded-i ((t (:foreground ,nano-base
                            :background ,nano-overlay0))))

   `(nano-default ((t  (:foreground ,nano-text))))

   `(nano-default-i ((t (:foreground ,nano-base
                              :background ,nano-text))))


   `(nano-salient ((t (:foreground ,nano-mauve))))

   `(nano-salient-i ((t (:foreground ,nano-base
                              :background ,nano-mauve))))



   `(nano-strong-i ((t (:foreground ,nano-base
                             :background ,nano-text
                             :weight normal))))

   `(nano-popout ((t (:foreground ,nano-peach))))

   `(nano-popout-i ((t (:foreground ,nano-base
                             :background ,nano-peach))))

   `(nano-critical ((t (:foreground ,nano-red
                             :weight normal))))

   `(nano-critical-i ((t (:foreground ,nano-base
                               :background ,nano-red
                               :weight normal))))

   ;; --- Header & mode line -------------------------------------------

   `(mode-line ((t (:foreground ,nano-base
                         :background ,nano-overlay0
                         :height 1
			 :underline t))))
   `(mode-line-highlight ((t (:inherit nano-popout))))
   `(mode-line-buffer-id ((t (:weight regular))))
   `(mode-line-emphasis  ((t (:weight regular))))

   `(mode-line-inactive ((t (:foreground ,nano-base
                                  :background ,nano-crust
                                  :height 1
				  :underline t))))

   `(header-line ((t (:foreground ,nano-text
                           :background ,nano-crust
                           :inherit nil
                           :box nil))))


   ;; --- Structural ---------------------------------------------------
   '(bold                        ((t (:inherit nano-strong :weight medium))))
   '(italic                      ((t (:slant italic))))
   ;; '(italic                      ((t (:inherit nano-faded))))
   '(bold-italic                 ((t (:inherit nano-strong :slant italic :weight medium))))
   '(region                      ((t (:inherit nano-subtle :distant-foreground unspecified))))
   '(fringe                      ((t (:inherit (nano-faded)))))
   '(hl-line                     ((t (:inherit highlight))))
   '(link                        ((t (:inherit nano-salient))))
   '(fixed-pitch                 ((t (:inherit default))))
   '(fixed-pitch-serif           ((t (:inherit default))))

   ;; --- Semantic -----------------------------------------------------
   '(shadow                        ((t (:inherit nano-faded))))
   '(success                       ((t (:inherit nano-salient))))
   '(warning                       ((t (:inherit nano-popout))))
   '(error                         ((t (:inherit nano-critical))))
   '(match                         ((t (:inherit nano-popout))))

   ;; --- General ------------------------------------------------------
   '(buffer-menu-buffer            ((t (:inherit nano-strong))))
   '(minibuffer-prompt             ((t (:inherit nano-strong))))
   '(isearch                       ((t (:inherit nano-strong))))
   '(isearch-fail                  ((t (:inherit nano-faded))))
   '(show-paren-match              ((t (:inherit nano-strong))))
   '(show-paren-mismatch           ((t (:inherit nano-critical))))
   '(lazy-highlight                ((t (:inherit nano-subtle))))
   '(trailing-whitespace           ((t (:inherit nano-subtle))))
   '(secondary-selection           ((t (:inherit nano-subtle))))
   '(completions-annotations       ((t (:inherit nano-faded))))
   '(completions-common-part       ((t (:inherit nano-strong))))
   '(completions-first-difference  ((t (:inherit nano-default))))
   '(tooltip                       ((t (:inherit nano-subtle))))
   '(read-multiple-choice-face     ((t (:inherit nano-strong))))
   '(nobreak-hyphen                ((t (:inherit nano-popout))))
   '(nobreak-space                 ((t (:inherit nano-popout))))
   '(help-argument-name            ((t (:inherit nano-faded))))
   '(tabulated-list-fake-header    ((t (:inherit nano-strong))))
   '(tool-bar                      ((t (:inherit (nano-subtle nano-faded)))))

   ;; --- TTY faces ----------------------------------------------------
   '(tty-menu-disabled-face        ((t (:inherit nano-faded-i))))
   '(tty-menu-enabled-face         ((t (:inherit nano-default-i))))
   '(tty-menu-selected-face        ((t (:inherit nano-salient-i))))

   ;; --- Tab bar ------------------------------------------------------
   '(tab-bar                       ((t (:inherit default))))
   '(tab-bar-tab                   ((t (:inherit default))))
   '(tab-bar-tab-inactive          ((t (:inherit nano-faded))))
   '(tab-line                      ((t (:inherit default))))

   ;; --- Line numbers -------------------------------------------------
   '(line-number                  ((t (:inherit nano-faded))))
   '(line-number-current-line     ((t (:inherit (nano-strong hl-line)))))
   `(line-number-major-tick       ((t (:inherit nano-default))))
   '(line-number-minor-tick       ((t (:inherit nano-faded))))

   ;; --- Diff HL (fringe mode) ----------------------------------------
   '(diff-hl-change                  ((t (:inherit nano-faded-i))))
   '(diff-hl-insert                  ((t (:inherit nano-salient-i))))
   '(diff-hl-delete                  ((t (:inherit nano-critical-i))))

   ;; --- Font lock ----------------------------------------------------
   '(font-lock-comment-face        ((t (:inherit nano-faded))))
   '(font-lock-doc-face            ((t (:inherit nano-faded))))
   '(font-lock-string-face         ((t (:inherit nano-faded))))
   '(font-lock-constant-face       ((t (:inherit nano-salient))))
   '(font-lock-warning-face        ((t (:inherit nano-popout))))
   '(font-lock-function-name-face  ((t (:inherit nano-strong))))
   '(font-lock-variable-name-face  ((t (:inherit nano-default))))
   '(font-lock-builtin-face        ((t (:inherit nano-salient))))
   '(font-lock-type-face           ((t (:inherit nano-salient))))
   '(font-lock-keyword-face        ((t (:inherit nano-salient))))

   ;; --- Custom edit --------------------------------------------------
   '(widget-field                  ((t (:inherit nano-subtle))))
   '(widget-button                 ((t (:inherit nano-strong))))
   '(widget-single-line-field      ((t (:inherit nano-subtle))))
   '(custom-group-subtitle         ((t (:inherit nano-strong))))
   '(custom-group-tag              ((t (:inherit nano-strong))))
   '(custom-group-tag-1            ((t (:inherit nano-strong))))
   '(custom-comment                ((t (:inherit nano-faded))))
   '(custom-comment-tag            ((t (:inherit nano-faded))))
   '(custom-changed                ((t (:inherit nano-salient))))
   '(custom-modified               ((t (:inherit nano-salient))))
   '(custom-face-tag               ((t (:inherit nano-strong))))
   '(custom-variable-tag           ((t (:inherit nano-strong))))
   '(custom-invalid                ((t (:inherit nano-popout))))
   '(custom-visibility             ((t (:inherit nano-salient))))
   '(custom-state                  ((t (:inherit nano-salient))))
   '(custom-link                   ((t (:inherit nano-salient))))
   '(custom-variable-obsolete      ((t (:inherit nano-faded))))

   ;; --- Company tooltip ----------------------------------------------
   '(company-tooltip                      ((t (:inherit nano-subtle))))
   '(company-tooltip-mouse                ((t (:inherit nano-faded-i))))
   '(company-tooltip-selection            ((t (:inherit nano-salient-i))))

   '(company-scrollbar-fg                 ((t (:inherit nano-default-i))))
   '(company-scrollbar-bg                 ((t (:inherit nano-faded-i))))

   '(company-tooltip-scrollbar-thumb      ((t (:inherit nano-default-i))))
   '(company-tooltip-scrollbar-track      ((t (:inherit nano-faded-i))))

   '(company-tooltip-common               ((t (:inherit nano-strong))))
   '(company-tooltip-common-selection     ((t (:inherit nano-salient-i
                                                :weight normal))))
   '(company-tooltip-annotation           ((t (:inherit nano-default))))
   '(company-tooltip-annotation-selection ((t (:inherit nano-subtle))))

   ;; --- Compilation --------------------------------------------------
   '(compilation-error ((t (:inherit nano-critical))))
   '(compilation-info ((t (:inherit nano-default))))
   '(compilation-warning ((t (:inherit nano-popout))))
   '(compilation-line-number ((t (:inherit nano-default))))
   '(compilation-column-number ((t (:inherit nano-default))))
   '(compilation-mode-line-run ((t (:inherit nano-default-i))))
   '(compilation-mode-line-exit ((t (:inherit nano-default-i))))
   '(compilation-mode-line-fail ((t (:inherit nano-critical))))

   ;; --- Buttons ------------------------------------------------------
   `(custom-button
     ((t (:foreground ,nano-overlay0
               :background ,nano-mantle
               :box nil))))

   `(custom-button-mouse
     ((t (:foreground ,nano-text
           :background ,nano-crust
               :box nil))))

   `(custom-button-pressed
     ((t (:foreground ,nano-base
           :background ,nano-text
               :box nil))))

   ;; --- Packages -----------------------------------------------------
   '(package-description            ((t (:inherit nano-default))))
   '(package-help-section-name      ((t (:inherit nano-default))))
   '(package-name                   ((t (:inherit nano-salient))))
   '(package-status-avail-obso      ((t (:inherit nano-faded))))
   '(package-status-available       ((t (:inherit nano-default))))
   '(package-status-built-in        ((t (:inherit nano-salient))))
   '(package-status-dependency      ((t (:inherit nano-salient))))
   '(package-status-disabled        ((t (:inherit nano-faded))))
   '(package-status-external        ((t (:inherit nano-default))))
   '(package-status-held            ((t (:inherit nano-default))))
   '(package-status-incompat        ((t (:inherit nano-faded))))
   '(package-status-installed       ((t (:inherit nano-salient))))
   '(package-status-new             ((t (:inherit nano-default))))
   '(package-status-unsigned        ((t (:inherit nano-default))))

   ;; --- Info ---------------------------------------------------------
   '(info-node                      ((t (:inherit nano-strong))))
   '(info-menu-header               ((t (:inherit nano-strong))))
   '(info-header-node               ((t (:inherit nano-default))))
   '(info-index-match               ((t (:inherit nano-salient))))
   '(Info-quoted                    ((t (:inherit nano-faded))))
   '(info-title-1                   ((t (:inherit nano-strong))))
   '(info-title-2                   ((t (:inherit nano-strong))))
   '(info-title-3                   ((t (:inherit nano-strong))))
   '(info-title-4                   ((t (:inherit nano-strong))))

   ;; --- Helpful ------------------------------------------------------
   '(helpful-heading                ((t (:inherit nano-strong))))

   ;; --- Nano modeline ------------------------------------------------
;;   '(nano-modeline-active               ((t (:inherit nano-subtle))))
   '(nano-modeline-active-name          ((t (:inherit (nano-strong nano-modeline-active)))))
   '(nano-modeline-active-primary       ((t (:inherit (nano-default nano-modeline-active)))))
   '(nano-modeline-active-secondary     ((t (:inherit (nano-faded nano-modeline-active)))))
   '(nano-modeline-active-status-RO     ((t (:inherit (nano-subtle nano-strong)))))
   '(nano-modeline-active-status-RW     ((t (:inherit (nano-faded-i nano-strong)))))
   '(nano-modeline-active-status-**     ((t (:inherit (nano-popout-i nano-strong)))))

;;   '(nano-modeline-inactive             ((t (:inherit nano-subtle))))
   '(nano-modeline-inactive-name        ((t (:inherit (nano-faded nano-modeline-inactive)))))
   '(nano-modeline-inactive-primary     ((t (:inherit (nano-faded nano-modeline-inactive)))))
   '(nano-modeline-inactive-secondary   ((t (:inherit (nano-faded nano-modeline-inactive)))))
   '(nano-modeline-inactive-status-RO   ((t (:inherit (nano-faded
                                                       nano-strong nano-modeline-inactive)))))
   '(nano-modeline-inactive-status-RW   ((t (:inherit (nano-faded
                                                       nano-strong nano-modeline-inactive)))))
   '(nano-modeline-inactive-status-**   ((t (:inherit (nano-popout
                                                       nano-strong nano-modeline-inactive)))))

   ;; --- nano agenda ---------------------------------------------------------
   '(nano-agenda-button               ((t (:inherit (nano-faded)))))
   '(nano-agenda-day-name             ((t (:inherit (nano-faded)))))
   '(nano-agenda-default              ((t (:inherit (nano-default)))))
   '(nano-agenda-holidays             ((t (:inherit (nano-faded)))))
   '(nano-agenda-month-name           ((t (:inherit (nano-strong)))))
   '(nano-agenda-mouse                ((t (:inherit (nano-highlight)))))
   '(nano-agenda-outday               ((t (:inherit (nano-subtle-i)))))
   '(nano-agenda-selected             ((t (:inherit (nano-default-i)))))
   '(nano-agenda-selected-today       ((t (:inherit (nano-popout-i nano-strong)))))
   '(nano-agenda-today                ((t (:inherit (nano-popout nano-strong)))))
   '(nano-agenda-weekend              ((t (:inherit (nano-faded)))))

   ;; --- EPA ----------------------------------------------------------
   '(epa-field-body                 ((t (:inherit nano-default))))
   '(epa-field-name                 ((t (:inherit nano-strong))))
   '(epa-mark                       ((t (:inherit nano-salient))))
   '(epa-string                     ((t (:inherit nano-popout))))
   '(epa-validity-disabled          ((t (:inherit nano-faded))))
   '(epa-validity-high              ((t (:inherit nano-strong))))
   '(epa-validity-medium            ((t (:inherit nano-default))))
   '(epa-validity-low               ((t (:inherit nano-faded))))

   ;; --- Popup --------------------------------------------------------
   '(popup-face                       ((t (:inherit highlight))))
   '(popup-isearch-match              ((t (:inherit nano-popout))))
   '(popup-menu-face                  ((t (:inherit nano-subtle))))
   '(popup-menu-mouse-face            ((t (:inherit nano-faded-i))))
   '(popup-menu-selection-face        ((t (:inherit nano-salient-i))))
   '(popup-menu-summary-face          ((t (:inherit nano-faded))))
   '(popup-scroll-bar-background-face ((t (:inherit nano-subtle))))
   '(popup-scroll-bar-foreground-face ((t (:inherit nano-subtle))))
   '(popup-summary-face               ((t (:inherit nano-faded))))
   '(popup-tip-face                   ((t (:inherit nano-popout-i))))

   ;; --- Diff ---------------------------------------------------------
   '(diff-header                    ((t (:inherit nano-faded))))
   '(diff-file-header               ((t (:inherit nano-strong))))
   '(diff-context                   ((t (:inherit nano-default))))
   '(diff-removed                   ((t (:inherit nano-faded))))
   '(diff-changed                   ((t (:inherit nano-popout))))
   '(diff-added                     ((t (:inherit nano-salient))))
   '(diff-refine-added              ((t (:inherit (nano-salient
                                                   nano-strong)))))
   '(diff-refine-changed            ((t (:inherit nano-popout))))
   '(diff-refine-removed            ((t (:inherit nano-faded
                                         :strike-through t))))

   ;; --- icomplete --------------------------------------------------------
   '(icomplete-first-match          ((t (:inherit nano-strong))))
   '(icomplete-selected-match       ((t (:inherit nano-strong))))
   '(icomplete-section              ((t (:inherit nano-strong))))

   ;; --- Vertico --------------------------------------------------------
   '(vertico-current                       ((t (:inherit (nano-strong
                                                          nano-subtle)))))
   '(vertico-group-separator               ((t (:inherit nano-faded))))
   '(vertico-group-title                   ((t (:inherit nano-faded))))
   '(vertico-multiline                     ((t (:inherit nano-faded))))

   ;; --- Citar --------------------------------------------------------
   '(citar                          ((t (:inherit nano-faded))))
   '(citar-highlight                ((t (:inherit nano-default))))

   ;; --- Corfu --------------------------------------------------------
   '(corfu-annotations              ((t (:inherit nano-faded))))
   '(corfu-bar                      ((t (:inherit nano-default-i))))
   '(corfu-border                   ((t (:inherit nano-default-i))))
   '(corfu-current                  ((t (:inherit highlight))))
   '(corfu-default                  ((t (:inherit nano-subtle))))
   '(corfu-deprecated               ((t (:inherit nano-faded))))
   '(corfu-echo                     ((t (:inherit nano-faded))))

   ;; --- Orderless ----------------------------------------------------
   '(orderless-match-face-0         ((t (:inherit (nano-salient
                                                   nano-strong)))))
   '(orderless-match-face-1         ((t (:inherit (nano-strong)))))
   '(orderless-match-face-2         ((t (:inherit (nano-strong)))))
   '(orderless-match-face-3         ((t (:inherit (nano-strong)))))

   ;; --- Message ------------------------------------------------------
   '(message-cited-text-1           ((t (:inherit nano-faded))))
   '(message-cited-text-2           ((t (:inherit nano-faded))))
   '(message-cited-text-3           ((t (:inherit nano-faded))))
   '(message-cited-text-4           ((t (:inherit nano-faded))))
   '(message-cited-text             ((t (:inherit nano-faded))))
   '(message-header-cc              ((t (:inherit nano-default))))
   '(message-header-name            ((t (:inherit nano-strong))))
   '(message-header-newsgroups      ((t (:inherit nano-default))))
   '(message-header-other           ((t (:inherit nano-default))))
   '(message-header-subject         ((t (:inherit nano-salient))))
   '(message-header-to              ((t (:inherit nano-salient))))
   '(message-header-xheader         ((t (:inherit nano-default))))
   '(message-mml                    ((t (:inherit nano-popout))))
   '(message-separator              ((t (:inherit nano-faded))))

   ;; --- Outline ------------------------------------------------------
   '(outline-1                      ((t (:inherit nano-strong))))
   '(outline-2                      ((t (:inherit nano-strong))))
   '(outline-3                      ((t (:inherit nano-strong))))
   '(outline-4                      ((t (:inherit nano-strong))))
   '(outline-5                      ((t (:inherit nano-strong))))
   '(outline-6                      ((t (:inherit nano-strong))))
   '(outline-7                      ((t (:inherit nano-strong))))
   '(outline-8                      ((t (:inherit nano-strong))))

   ;; --- Fly spell ----------------------------------------------------
   '(flyspell-duplicate             ((t (:inherit nano-popout
                                         :underline t))))
   '(flyspell-incorrect             ((t (:inherit nano-popout
                                         :underline t))))

   ;; --- Org agenda ---------------------------------------------------
   '(org-agenda-calendar-event      ((t (:inherit nano-default))))
   '(org-agenda-calendar-sexp       ((t (:inherit nano-salient))))
   '(org-agenda-clocking            ((t (:inherit nano-faded))))
   '(org-agenda-column-dateline     ((t (:inherit nano-faded))))
   '(org-agenda-current-time        ((t (:inherit (nano-strong
                                                   nano-salient)))))
   '(org-agenda-date                ((t (:inherit nano-strong))))
   '(org-agenda-date-today          ((t (:inherit (nano-salient
                                                   nano-strong)))))
   '(org-agenda-date-weekend        ((t (:inherit nano-faded))))
   '(org-agenda-diary               ((t (:inherit nano-faded))))
   '(org-agenda-dimmed-todo-face    ((t (:inherit nano-faded))))
   '(org-agenda-done                ((t (:inherit nano-faded))))
   '(org-agenda-filter-category     ((t (:inherit nano-faded))))
   '(org-agenda-filter-effort       ((t (:inherit nano-faded))))
   '(org-agenda-filter-regexp       ((t (:inherit nano-faded))))
   '(org-agenda-filter-tags         ((t (:inherit nano-faded))))
   '(org-agenda-property-face       ((t (:inherit nano-faded))))
   '(org-agenda-restriction-lock    ((t (:inherit nano-faded))))
   '(org-agenda-structure           ((t (:inherit nano-strong))))

   ;; --- Org ----------------------------------------------------------
   '(org-archived                            ((t (:inherit nano-faded))))
   '(org-block                               ((t (:inherit highlight))))
   `(org-block-begin-line                    ((t (:inherit nano-faded
						  :background ,nano-mantle
						  :box (:line-width (1 . 6)
							:color ,nano-mantle)))))
   `(org-block-end-line                      ((t (:inherit org-block-begin-line))))
   '(org-checkbox                            ((t (:inherit nano-faded))))
   '(org-checkbox-statistics-done            ((t (:inherit nano-faded))))
   '(org-checkbox-statistics-todo            ((t (:inherit nano-faded))))
   '(org-clock-overlay                       ((t (:inherit nano-faded))))
   '(org-code                                ((t (:inherit (nano-salient
							    nano-mono)))))
   '(org-column                              ((t (:inherit nano-faded))))
   '(org-column-title                        ((t (:inherit nano-faded))))
   '(org-date                                ((t (:inherit nano-faded))))
   '(org-date-selected                       ((t (:inherit nano-faded))))
   '(org-default                             ((t (:inherit nano-faded))))
   '(org-document-info                       ((t (:inherit nano-faded))))
   '(org-document-info-keyword               ((t (:inherit nano-faded))))
   '(org-document-title                      ((t (:inherit nano-strong
						  :weight bold
						  :height 2.2))))
   '(org-done                                ((t (:inherit (nano-faded
							    nano-mono)))))
   '(org-drawer                              ((t (:inherit nano-faded))))
   '(org-ellipsis                            ((t (:inherit nano-faded))))
   '(org-footnote                            ((t (:inherit nano-faded))))
   '(org-formula                             ((t (:inherit nano-faded))))
   '(org-headline-done                       ((t (:inherit nano-faded
						  :strike-through t))))
   '(org-latex-and-related                   ((t (:inherit (nano-faded
							    nano-mono)))))
   '(org-level-1                             ((t (:inherit nano-strong
						  :weight bold
						  :height 1.6))))
   '(org-level-2                             ((t (:inherit nano-strong
						  :weight bold
						  :height 1.25))))
   '(org-level-3                             ((t (:inherit nano-strong
						  :height 1.1))))
   '(org-level-4                             ((t (:inherit nano-strong))))
   '(org-level-5                             ((t (:inherit nano-strong))))
   '(org-level-6                             ((t (:inherit nano-strong))))
   '(org-level-7                             ((t (:inherit nano-strong))))
   '(org-level-8                             ((t (:inherit nano-strong))))
   '(org-link                                ((t (:inherit nano-salient))))
   '(org-list-dt                             ((t (:inherit nano-faded))))
   '(org-macro                               ((t (:inherit nano-faded))))
   '(org-meta-line                           ((t (:inherit nano-faded))))
   '(org-mode-line-clock                     ((t (:inherit nano-faded))))
   '(org-mode-line-clock-overrun             ((t (:inherit nano-faded))))
   '(org-priority                            ((t (:inherit (nano-faded
							    nano-mono)))))
   '(org-property-value                      ((t (:inherit nano-faded))))
   '(org-quote                               ((t (:inherit highlight
						  :extend t))))
   '(org-scheduled                           ((t (:inherit nano-faded))))
   '(org-scheduled-previously                ((t (:inherit nano-faded))))
   '(org-scheduled-today                     ((t (:inherit nano-faded))))
   '(org-sexp-date                           ((t (:inherit nano-faded))))
   '(org-special-keyword                     ((t (:inherit nano-faded))))
   '(org-table                               ((t (:inherit nano-faded))))
   '(org-tag                                 ((t (:inherit nano-popout))))
   '(org-tag-group                           ((t (:inherit nano-faded))))
   '(org-target                              ((t (:inherit nano-faded))))
   '(org-time-grid                           ((t (:inherit nano-faded))))
   '(org-todo                                ((t (:inherit (nano-salient
							    nano-mono)))))
   '(org-upcoming-deadline                   ((t (:inherit nano-popout))))
   '(org-verbatim                            ((t (:inherit (nano-popout
							    nano-mono)))))
   '(org-verse                               ((t (:inherit nano-faded))))
   '(org-warning                             ((t (:inherit nano-popout))))

   ;; --- Mu4e ---------------------------------------------------------
   '(mu4e-attach-number-face                ((t (:inherit nano-strong))))
   '(mu4e-cited-1-face                       ((t (:inherit nano-faded))))
   '(mu4e-cited-2-face                       ((t (:inherit nano-faded))))
   '(mu4e-cited-3-face                       ((t (:inherit nano-faded))))
   '(mu4e-cited-4-face                       ((t (:inherit nano-faded))))
   '(mu4e-cited-5-face                       ((t (:inherit nano-faded))))
   '(mu4e-cited-6-face                       ((t (:inherit nano-faded))))
   '(mu4e-cited-7-face                       ((t (:inherit nano-faded))))
   '(mu4e-compose-header-face                ((t (:inherit nano-faded))))
   '(mu4e-compose-separator-face             ((t (:inherit nano-faded))))
   '(mu4e-contact-face                     ((t (:inherit nano-salient))))
   '(mu4e-context-face                       ((t (:inherit nano-faded))))
   '(mu4e-draft-face                         ((t (:inherit nano-faded))))
   '(mu4e-flagged-face                     ((t (:inherit nano-salient))))
   '(mu4e-footer-face                        ((t (:inherit nano-faded))))
   '(mu4e-forwarded-face                   ((t (:inherit nano-default))))
   '(mu4e-header-face                      ((t (:inherit nano-default))))
   '(mu4e-header-highlight-face               ((t (:inherit highlight))))
   '(mu4e-header-key-face                   ((t (:inherit nano-strong))))
   '(mu4e-header-marks-face                  ((t (:inherit nano-faded))))
   '(mu4e-header-title-face                 ((t (:inherit nano-strong))))
   '(mu4e-header-field-face                 ((t (:inherit nano-strong))))
   '(mu4e-header-value-face                ((t (:inherit nano-default))))
   '(mu4e-highlight-face                    ((t (:inherit nano-popout))))
   '(mu4e-link-face                        ((t (:inherit nano-salient))))
   '(mu4e-modeline-face                      ((t (:inherit nano-faded))))
   '(mu4e-moved-face                         ((t (:inherit nano-faded))))
   '(mu4e-ok-face                            ((t (:inherit nano-faded))))
   '(mu4e-region-code                        ((t (:inherit nano-faded))))
   '(mu4e-replied-face                     ((t (:inherit nano-default))))
   '(mu4e-special-header-value-face        ((t (:inherit nano-default))))
   '(mu4e-system-face                        ((t (:inherit nano-faded))))
   '(mu4e-related-face                       ((t (:inherit nano-faded))))
   '(mu4e-title-face                        ((t (:inherit nano-strong))))
   '(mu4e-trashed-face                       ((t (:inherit nano-faded))))
   '(mu4e-unread-face                       ((t (:inherit nano-strong))))
   '(mu4e-url-number-face                    ((t (:inherit nano-faded))))
   '(mu4e-view-body-face                   ((t (:inherit nano-default))))
   '(mu4e-warning-face                      ((t (:inherit nano-popout))))

   ;; --- GNUS ---------------------------------------------------------
   '(gnus-button                            ((t (:inherit nano-salient))))
   '(gnus-cite-1                            ((t (:inherit nano-faded))))
   '(gnus-cite-10                           ((t (:inherit nano-faded))))
   '(gnus-cite-11                           ((t (:inherit nano-faded))))
   '(gnus-cite-2                            ((t (:inherit nano-faded))))
   '(gnus-cite-3                            ((t (:inherit nano-faded))))
   '(gnus-cite-4                            ((t (:inherit nano-faded))))
   '(gnus-cite-5                            ((t (:inherit nano-faded))))
   '(gnus-cite-6                            ((t (:inherit nano-faded))))
   '(gnus-cite-7                            ((t (:inherit nano-faded))))
   '(gnus-cite-8                            ((t (:inherit nano-faded))))
   '(gnus-cite-9                            ((t (:inherit nano-faded))))
   '(gnus-cite-attribution                  ((t (:inherit nano-faded))))
   '(gnus-emphasis-bold                     ((t (:inherit nano-faded))))
   '(gnus-emphasis-bold-italic              ((t (:inherit nano-faded))))
   '(gnus-emphasis-highlight-words          ((t (:inherit nano-faded))))
   '(gnus-emphasis-italic                   ((t (:inherit nano-faded))))
   '(gnus-emphasis-strikethru               ((t (:inherit nano-faded))))
   '(gnus-emphasis-underline                ((t (:inherit nano-faded))))
   '(gnus-emphasis-underline-bold           ((t (:inherit nano-faded))))
   '(gnus-emphasis-underline-bold-italic    ((t (:inherit nano-faded))))
   '(gnus-emphasis-underline-italic         ((t (:inherit nano-faded))))
   '(gnus-group-mail-1                      ((t (:inherit nano-faded))))
   '(gnus-group-mail-1-empty                ((t (:inherit nano-faded))))
   '(gnus-group-mail-2                      ((t (:inherit nano-faded))))
   '(gnus-group-mail-2-empty                ((t (:inherit nano-faded))))
   '(gnus-group-mail-3                      ((t (:inherit nano-faded))))
   '(gnus-group-mail-3-empty                ((t (:inherit nano-faded))))
   '(gnus-group-mail-low                    ((t (:inherit nano-faded))))
   '(gnus-group-mail-low-empty              ((t (:inherit nano-faded))))
   '(gnus-group-news-1                      ((t (:inherit nano-faded))))
   '(gnus-group-news-1-empty                ((t (:inherit nano-faded))))
   '(gnus-group-news-2                      ((t (:inherit nano-faded))))
   '(gnus-group-news-2-empty                ((t (:inherit nano-faded))))
   '(gnus-group-news-3                      ((t (:inherit nano-faded))))
   '(gnus-group-news-3-empty                ((t (:inherit nano-faded))))
   '(gnus-group-news-4                      ((t (:inherit nano-faded))))
   '(gnus-group-news-4-empty                ((t (:inherit nano-faded))))
   '(gnus-group-news-5                      ((t (:inherit nano-faded))))
   '(gnus-group-news-5-empty                ((t (:inherit nano-faded))))
   '(gnus-group-news-6                      ((t (:inherit nano-faded))))
   '(gnus-group-news-6-empty                ((t (:inherit nano-faded))))
   '(gnus-group-news-low                    ((t (:inherit nano-faded))))
   '(gnus-group-news-low-empty              ((t (:inherit nano-faded))))

   '(gnus-header-content                    ((t (:inherit nano-faded))))
   '(gnus-header-from                       ((t (:inherit nano-strong))))
   '(gnus-header-name                       ((t (:inherit nano-strong))))
   '(gnus-header-newsgroups                 ((t (:inherit nano-faded))))
   '(gnus-header-subject                    ((t (:inherit nano-default))))

   '(gnus-signature                         ((t (:inherit nano-faded))))
   '(gnus-splash                            ((t (:inherit nano-faded))))
   '(gnus-summary-cancelled                 ((t (:inherit nano-faded))))
   '(gnus-summary-high-ancient              ((t (:inherit nano-faded))))
   '(gnus-summary-high-read                 ((t (:inherit nano-faded))))
   '(gnus-summary-high-ticked               ((t (:inherit nano-faded))))
   '(gnus-summary-high-undownloaded         ((t (:inherit nano-faded))))
   '(gnus-summary-high-unread               ((t (:inherit nano-faded))))
   '(gnus-summary-low-ancient               ((t (:inherit nano-faded))))
   '(gnus-summary-low-read                  ((t (:inherit nano-faded))))
   '(gnus-summary-low-ticked                ((t (:inherit nano-faded))))
   '(gnus-summary-low-undownloaded          ((t (:inherit nano-faded))))
   '(gnus-summary-low-unread                ((t (:inherit nano-faded))))
   '(gnus-summary-normal-ancient            ((t (:inherit nano-faded))))
   '(gnus-summary-normal-read               ((t (:inherit nano-faded))))
   '(gnus-summary-normal-ticked             ((t (:inherit nano-faded))))
   '(gnus-summary-normal-undownloaded       ((t (:inherit nano-faded))))
   '(gnus-summary-normal-unread             ((t (:inherit nano-faded))))
   '(gnus-summary-selected                  ((t (:inherit nano-faded))))

   ;; --- Marginalia ---------------------------------------------------
   '(marginalia-archive                     ((t (:inherit nano-faded))))
   '(marginalia-char                        ((t (:inherit nano-faded))))
   '(marginalia-date                        ((t (:inherit nano-faded))))
   '(marginalia-documentation               ((t (:inherit nano-faded))))
   '(marginalia-file-name                   ((t (:inherit nano-faded))))
   '(marginalia-file-owner                  ((t (:inherit nano-faded))))
   '(marginalia-file-priv-dir               ((t (:inherit nano-faded))))
   '(marginalia-file-priv-exec              ((t (:inherit nano-faded))))
   '(marginalia-file-priv-link              ((t (:inherit nano-faded))))
   '(marginalia-file-priv-no                ((t (:inherit nano-faded))))
   '(marginalia-file-priv-other             ((t (:inherit nano-faded))))
   '(marginalia-file-priv-rare              ((t (:inherit nano-faded))))
   '(marginalia-file-priv-read              ((t (:inherit nano-faded))))
   '(marginalia-file-priv-write             ((t (:inherit nano-faded))))
   '(marginalia-function                    ((t (:inherit nano-faded))))
   '(marginalia-installed                   ((t (:inherit nano-faded))))
   '(marginalia-key                         ((t (:inherit nano-faded))))
   '(marginalia-lighter                     ((t (:inherit nano-faded))))
   '(marginalia-list                        ((t (:inherit nano-faded))))
   '(marginalia-mode                        ((t (:inherit nano-faded))))
   '(marginalia-modified                    ((t (:inherit nano-faded))))
   '(marginalia-null                        ((t (:inherit nano-faded))))
   '(marginalia-number                      ((t (:inherit nano-faded))))
   '(marginalia-off                         ((t (:inherit nano-faded))))
   '(marginalia-on                          ((t (:inherit nano-faded))))
   '(marginalia-size                        ((t (:inherit nano-faded))))
   '(marginalia-string                      ((t (:inherit nano-faded))))
   '(marginalia-symbol                      ((t (:inherit nano-faded))))
   '(marginalia-true                        ((t (:inherit nano-faded))))
   '(marginalia-type                        ((t (:inherit nano-faded))))
   '(marginalia-value                       ((t (:inherit nano-faded))))
   '(marginalia-version                     ((t (:inherit nano-faded))))

   ;; --- Elfeed -------------------------------------------------------
    '(elfeed-log-date-face                    ((t (:inherit nano-faded))))
    '(elfeed-log-info-level-face            ((t (:inherit nano-default))))
    '(elfeed-log-debug-level-face           ((t (:inherit nano-default))))
    '(elfeed-log-warn-level-face             ((t (:inherit nano-popout))))
    '(elfeed-log-error-level-face            ((t (:inherit nano-popout))))
    '(elfeed-search-tag-face                  ((t (:inherit nano-faded))))
    '(elfeed-search-date-face                 ((t (:inherit nano-faded))))
    '(elfeed-search-feed-face               ((t (:inherit nano-salient))))
    '(elfeed-search-filter-face               ((t (:inherit nano-faded))))
    '(elfeed-search-last-update-face        ((t (:inherit nano-salient))))
    '(elfeed-search-title-face              ((t (:inherit nano-default))))
    '(elfeed-search-tag-face                  ((t (:inherit nano-faded))))
    '(elfeed-search-unread-count-face        ((t (:inherit nano-strong))))
    '(elfeed-search-unread-title-face        ((t (:inherit nano-strong))))

    ;; --- Deft --------------------------------------------------------
    '(deft-filter-string-error-face         ((t (:inherit nano-popout))))
    '(deft-filter-string-face              ((t (:inherit nano-default))))
    '(deft-header-face                     ((t (:inherit nano-salient))))
    '(deft-separator-face                    ((t (:inherit nano-faded))))
    '(deft-summary-face                      ((t (:inherit nano-faded))))
    '(deft-time-face                       ((t (:inherit nano-salient))))
    '(deft-title-face                       ((t (:inherit nano-strong))))

    ;; --- imenu-list ---------------------------------------------------
    '(imenu-list-entry-face                 ((t (:inherit nano-default))))
    '(imenu-list-entry-face-0                ((t (:inherit nano-strong))))
    '(imenu-list-entry-face-1               ((t ( ))))
    '(imenu-list-entry-face-2               ((t ( ))))
    '(imenu-list-entry-face-3               ((t ( ))))
    '(imenu-list-entry-subalist-face-0      ((t (:inherit nano-strong))))
    '(imenu-list-entry-subalist-face-1      ((t ( ))))
    '(imenu-list-entry-subalist-face-2      ((t ( ))))
    '(imenu-list-entry-subalist-face-3      ((t ( ))))

    ;; --- Restructured text -------------------------------------------
    '(rst-adornment                           ((t (:inherit nano-faded))))
    '(rst-block                             ((t (:inherit nano-default))))
    '(rst-comment                             ((t (:inherit nano-faded))))
    '(rst-definition                        ((t (:inherit nano-salient))))
    '(rst-directive                         ((t (:inherit nano-salient))))
    '(rst-emphasis1                           ((t (:inherit nano-faded))))
    '(rst-emphasis2                          ((t (:inherit nano-strong))))
    '(rst-external                          ((t (:inherit nano-salient))))
    '(rst-level-1                            ((t (:inherit nano-strong))))
    '(rst-level-2                            ((t (:inherit nano-strong))))
    '(rst-level-3                            ((t (:inherit nano-strong))))
    '(rst-level-4                            ((t (:inherit nano-strong))))
    '(rst-level-5                            ((t (:inherit nano-strong))))
    '(rst-level-6                            ((t (:inherit nano-strong))))
    '(rst-literal                           ((t (:inherit nano-salient))))
    '(rst-reference                         ((t (:inherit nano-salient))))
    '(rst-transition                        ((t (:inherit nano-default))))

    ;; --- Elpher ----------------------------------------------------
    '(elpher-gemini-heading1                 ((t (:inherit nano-strong))))
    '(elpher-gemini-heading2                 ((t (:inherit nano-strong))))
    '(elpher-gemini-heading3                 ((t (:inherit nano-strong))))
 
    ;; ---SHR ---------------------------------------------------------
    '(shr-abbreviation                    ((t (:inherit nano-popout))))
    '(shr-text                            ((t (:inherit nano-default))))
    '(shr-h1                              ((t (:inherit nano-strong))))
    '(shr-h2                              ((t (:inherit nano-strong))))
    '(shr-h3                              ((t (:inherit nano-strong))))
    '(shr-h4                              ((t (:inherit nano-strong))))
    '(shr-h5                              ((t (:inherit nano-strong))))
    '(shr-h6                              ((t (:inherit nano-strong))))
    '(shr-link                           ((t (:inherit nano-salient))))
    '(shr-selected-link      ((t (:inherit (nano-salient nano-subtle)))))
    '(shr-strike-through                   ((t (:inherit nano-faded))))

    ;; --- Markdown ----------------------------------------------------
    '(markdown-blockquote-face              ((t (:inherit nano-default))))
    '(markdown-bold-face                     ((t (:inherit nano-strong))))
    '(markdown-code-face                    ((t (:inherit nano-default))))
    '(markdown-comment-face                   ((t (:inherit nano-faded))))
    '(markdown-footnote-marker-face         ((t (:inherit nano-default))))
    '(markdown-footnote-text-face           ((t (:inherit nano-default))))
    '(markdown-gfm-checkbox-face            ((t (:inherit nano-default))))
    '(markdown-header-delimiter-face          ((t (:inherit nano-faded))))
    '(markdown-header-face                   ((t (:inherit nano-strong))))
    '(markdown-header-face-1                 ((t (:inherit nano-strong))))
    '(markdown-header-face-2                 ((t (:inherit nano-strong))))
    '(markdown-header-face-3                 ((t (:inherit nano-strong))))
    '(markdown-header-face-4                 ((t (:inherit nano-strong))))
    '(markdown-header-face-5                 ((t (:inherit nano-strong))))
    '(markdown-header-face-6                ((t (:inherit nano-strong))))
    '(markdown-header-rule-face             ((t (:inherit nano-default))))
    '(markdown-highlight-face               ((t (:inherit nano-default))))
    '(markdown-hr-face                      ((t (:inherit nano-default))))
    '(markdown-html-attr-name-face          ((t (:inherit nano-default))))
    '(markdown-html-attr-value-face         ((t (:inherit nano-default))))
    '(markdown-html-entity-face             ((t (:inherit nano-default))))
    '(markdown-html-tag-delimiter-face      ((t (:inherit nano-default))))
    '(markdown-html-tag-name-face           ((t (:inherit nano-default))))
    '(markdown-inline-code-face              ((t (:inherit nano-popout))))
    '(markdown-italic-face                    ((t (:inherit nano-faded))))
    '(markdown-language-info-face           ((t (:inherit nano-default))))
    '(markdown-language-keyword-face        ((t (:inherit nano-default))))
    '(markdown-line-break-face              ((t (:inherit nano-default))))
    '(markdown-link-face                    ((t (:inherit nano-salient))))
    '(markdown-link-title-face              ((t (:inherit nano-default))))
    '(markdown-list-face                      ((t (:inherit nano-faded))))
    '(markdown-markup-face                    ((t (:inherit nano-faded))))
    '(markdown-math-face                    ((t (:inherit nano-default))))
    '(markdown-metadata-key-face              ((t (:inherit nano-faded))))
    '(markdown-metadata-value-face            ((t (:inherit nano-faded))))
    '(markdown-missing-link-face            ((t (:inherit nano-default))))
    '(markdown-plain-url-face               ((t (:inherit nano-default))))
    '(markdown-pre-face                     ((t (:inherit nano-default))))
    '(markdown-reference-face               ((t (:inherit nano-salient))))
    '(markdown-strike-through-face            ((t (:inherit nano-faded))))
    '(markdown-table-face                   ((t (:inherit nano-default))))
    '(markdown-url-face                     ((t (:inherit nano-salient))))

    ;; --- Magit (WIP) ---------------------------------------------------
    '(magit-blame-highlight                  ((t (:inherit (highlight)))))
    '(magit-diff-added-highlight             ((t (:inherit (highlight nano-salient nano-strong)))))
    '(magit-diff-base-highlight              ((t (:inherit (highlight)))))
    '(magit-diff-context-highlight           ((t (:inherit (highlight nano-faded)))))
    '(magit-diff-file-heading-highlight      ((t (:inherit (highlight nano-strong)))))
    '(magit-diff-hunk-heading-highlight      ((t (:inherit (nano-default)))))
    '(magit-diff-our-highlight               ((t (:inherit (highlight)))))
    '(magit-diff-removed-highlight           ((t (:inherit (highlight nano-popout nano-strong)))))
    '(magit-diff-revision-summary-highlight  ((t (:inherit ()))))
    '(magit-diff-their-highlight             ((t (:inherit (highlight)))))
    '(magit-section-highlight                ((t (:inherit (highlight)))))

    '(magit-blame-heading                    ((t (:inherit (nano-subtle nano-strong)))))
    '(magit-diff-conflict-heading            ((t (:inherit (nano-subtle nano-strong)))))
    '(magit-diff-file-heading                ((t (:inherit (nano-strong)))))
    '(magit-diff-hunk-heading                ((t (:inherit (nano-subtle nano-default)))))
    '(magit-diff-lines-heading               ((t (:inherit (nano-subtle nano-strong)))))
    '(magit-section-heading                  ((t (:inherit (nano-salient nano-strong)))))

    '(magit-bisect-bad                       ((t (:inherit nano-default))))
    '(magit-bisect-good                      ((t (:inherit nano-default))))
    '(magit-bisect-skip                      ((t (:inherit nano-default))))
    '(magit-blame-date                       ((t (:inherit nano-default))))
    '(magit-blame-dimmed                     ((t (:inherit nano-default))))
    '(magit-blame-hash                       ((t (:inherit nano-faded))))

    '(magit-blame-margin                     ((t (:inherit nano-default))))
    '(magit-blame-name                       ((t (:inherit nano-default))))
    '(magit-blame-summary                    ((t (:inherit nano-default))))

    '(magit-branch-current                   ((t (:inherit (nano-strong nano-salient)))))
    '(magit-branch-local                     ((t (:inherit nano-salient))))
    '(magit-branch-remote                    ((t (:inherit (nano-salient)))))
    '(magit-branch-remote-head               ((t (:inherit (nano-salient)))))
    '(magit-branch-upstream                  ((t (:inherit (nano-salient)))))

    '(magit-cherry-equivalent                ((t (:inherit nano-default))))
    '(magit-cherry-unmatched                 ((t (:inherit nano-default))))

    '(magit-diff-added                       ((t (:inherit (nano-salient nano-strong)))))
    '(magit-diff-base                        ((t (:inherit nano-default))))
    '(magit-diff-context                     ((t (:inherit nano-faded))))
    '(magit-diff-file-heading-selection      ((t (:inherit nano-default))))
    '(magit-diff-hunk-heading-selection      ((t (:inherit nano-default))))
    '(magit-diff-hunk-region                 ((t (:inherit nano-default))))
    '(magit-diff-lines-boundary              ((t (:inherit nano-default))))
    '(magit-diff-our                         ((t (:inherit nano-default))))
    '(magit-diff-removed                     ((t (:inherit (nano-popout nano-strong)))))
    '(magit-diff-revision-summary            ((t (:inherit nano-popout))))
    '(magit-diff-their                       ((t (:inherit nano-default))))
    '(magit-diff-whitespace-warning          ((t (:inherit nano-subtle))))
    '(magit-diffstat-added                   ((t (:inherit nano-default))))
    '(magit-diffstat-removed                 ((t (:inherit nano-default))))

    '(magit-dimmed                           ((t (:inherit nano-faded))))
    '(magit-filename                         ((t (:inherit nano-default))))
    '(magit-hash                             ((t (:inherit nano-faded))))
    '(magit-head                             ((t (:inherit nano-default))))
    '(magit-header-line                      ((t (:inherit nano-default))))
    '(magit-header-line-key                  ((t (:inherit nano-default))))
    '(magit-header-line-log-select           ((t (:inherit nano-default))))

    '(magit-keyword                          ((t (:inherit nano-salient))))
    '(magit-keyword-squash                   ((t (:inherit nano-salient))))

    '(magit-log-author                       ((t (:inherit nano-default))))
    '(magit-log-date                         ((t (:inherit nano-default))))
    '(magit-log-graph                        ((t (:inherit nano-default))))

    '(magit-mode-line-process                ((t (:inherit nano-default))))
    '(magit-mode-line-process-error          ((t (:inherit nano-critical))))

    '(magit-process-ng                       ((t (:inherit nano-default))))
    '(magit-process-ok                       ((t (:inherit nano-default))))

    '(magit-reflog-amend                     ((t (:inherit nano-default))))
    '(magit-reflog-checkout                  ((t (:inherit nano-default))))
    '(magit-reflog-cherry-pick               ((t (:inherit nano-default))))
    '(magit-reflog-commit                    ((t (:inherit nano-default))))
    '(magit-reflog-merge                     ((t (:inherit nano-default))))
    '(magit-reflog-other                     ((t (:inherit nano-default))))
    '(magit-reflog-rebase                    ((t (:inherit nano-default))))
    '(magit-reflog-remote                    ((t (:inherit nano-default))))
    '(magit-reflog-reset                     ((t (:inherit nano-default))))
    '(magit-refname                          ((t (:inherit nano-default))))
    '(magit-refname-pullreq                  ((t (:inherit nano-default))))
    '(magit-refname-stash                    ((t (:inherit nano-default))))
    '(magit-refname-wip                      ((t (:inherit nano-default))))

    '(magit-section-heading-selection        ((t (:inherit nano-default))))
    '(magit-section-secondary-heading        ((t (:inherit nano-default))))
    '(magit-sequence-done                    ((t (:inherit nano-default))))
    '(magit-sequence-drop                    ((t (:inherit nano-default))))
    '(magit-sequence-exec                    ((t (:inherit nano-default))))
    '(magit-sequence-head                    ((t (:inherit nano-default))))
    '(magit-sequence-onto                    ((t (:inherit nano-default))))
    '(magit-sequence-part                    ((t (:inherit nano-default))))
    '(magit-sequence-pick                    ((t (:inherit nano-default))))
    '(magit-sequence-stop                    ((t (:inherit nano-default))))

    '(magit-signature-bad                    ((t (:inherit nano-default))))
    '(magit-signature-error                  ((t (:inherit nano-default))))
    '(magit-signature-expired                ((t (:inherit nano-default))))
    '(magit-signature-expired-key            ((t (:inherit nano-default))))
    '(magit-signature-good                   ((t (:inherit nano-default))))
    '(magit-signature-revoked                ((t (:inherit nano-default))))
    '(magit-signature-untrusted              ((t (:inherit nano-default))))

    '(magit-tag                              ((t (:inherit nano-strong))))

    ;; --- Transient ------------------------------------------------------
    ;; Set only faces that influence Magit.  See:
    ;; <https://github.com/rougier/nano-theme/issues/43>
    '(transient-value                        ((t (:inherit default))))

    ;; --- ANSI colors ----------------------------------------------------

    '(ansi-color-black                       ((t (:inherit nano-default))))
    '(ansi-color-bold                         ((t (:inherit nano-strong))))
    '(ansi-color-bright-black                 ((t (:inherit nano-strong))))
    '(ansi-color-faint                         ((t (:inherit nano-faded))))
    '(ansi-color-fast-blink                    ((t (:inherit nano-faded))))
    '(ansi-color-slow-blink                    ((t (:inherit nano-faded))))
    '(ansi-color-inverse                   ((t (:inherit nano-default-i))))
    '(ansi-color-italic                            ((t (:inherit italic))))
    '(ansi-color-underline                     ((t (:inherit nano-faded))))
    '(ansi-color-blue           ((t (:foreground "#42A5F5")))) ;; material color blue L400
    '(ansi-color-bright-blue    ((t (:background "#BBDEFB")))) ;; material color blue L100
    '(ansi-color-cyan           ((t (:foreground "#26C6DA")))) ;; material color cyan L400
    '(ansi-color-bright-cyan    ((t (:background "#B2EBF2")))) ;; material color cyan L100
    '(ansi-color-green          ((t (:foreground "#66BB6A")))) ;; material color green L400
    '(ansi-color-bright-green   ((t (:background "#C8E6C9")))) ;; material color green L100
    '(ansi-color-magenta        ((t (:foreground "#AB47BC")))) ;; material color purple L400
    '(ansi-color-bright-magenta ((t (:background "#E1BEE7")))) ;; material color purple L100
    '(ansi-color-red            ((t (:foreground "#EF5350")))) ;; material color red L400
    '(ansi-color-bright-red     ((t (:background "#FFCDD2")))) ;; material color red L100
    '(ansi-color-white          ((t (:inherit nano-subtle))))
    '(ansi-color-bright-white   ((t (:inherit default))))
    '(ansi-color-yellow         ((t (:foreground "#FFEE58")))) ;; material color yellow L400
    '(ansi-color-bright-yellow  ((t (:background "#FFF9C4")))) ;; material color yellow L100


    ;; --- Terminal ----------------------------------------------------
    '(term-bold        ((t (:inherit nano-strong))))
    '(term-color-black ((t (:inherit default))))
    '(term-color-blue ((t (:foreground "#42A5F5"        ;; material color blue L400
                           :background "#BBDEFB"))))    ;; material color blue L100
    '(term-color-cyan ((t (:foreground "#26C6DA"        ;; material color cyan L400
                           :background "#B2EBF2"))))    ;; material color cyan L100
    '(term-color-green ((t (:foreground "#66BB6A"       ;; material color green L400
                            :background "#C8E6C9"))))   ;; material color green L100
    '(term-color-magenta ((t (:foreground "#AB47BC"     ;; material color purple L400
                              :background "#E1BEE7")))) ;; material color purple L100
    '(term-color-red ((t (:foreground "#EF5350"         ;; material color red L400
                          :background "#FFCDD2"))))     ;; material color red L100
    '(term-color-yellow ((t (:foreground "#FFEE58"      ;; material color yellow L400
                             :background "#FFF9C4"))))  ;; material color yellow L100
    ))

  (dolist (buffer (list " *Minibuf-0*" " *Echo Area 0*"
                        " *Minibuf-1*" " *Echo Area 1*"))
    (when (get-buffer buffer)
      (with-current-buffer buffer
        (face-remap-add-relative 'default 'nano-faded))))
  (advice-remove 'frame-list #'nano-frame-list-advice-selected))

;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide 'nano-theme-support)
;;; nano-theme-support.el ends here
