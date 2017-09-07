;; Font setting
;; TODO: Add yethangul support
;; TODO: Fix emoji support
;; TODO: Tengwar fallback
(defun set-font (fontsz)
  ;;Default font - Source Code Pro
  (set-face-attribute 'default nil
                      :family "Source Code Pro"
                      :height fontsz
                      :weight 'normal
                      :width 'normal)

  ;;Hangul fallback - Noto Sans CJK KR
  ;;TODO: Find way to emulate fixedwidth
  (set-fontset-font "fontset-default"
                    '(#xAC00 . #xD7A3)
                    (font-spec
                     :family "Noto Sans CJK KR"
                     :height fontsz
                     :weight 'normal
                     :width 'normal))

  (let ((emojifont (font-spec
                   :family "Noto Emoji"
                   :height: fontsz
                   :weight: 'normal
                   :width 'normal)))
    ;; Miscellaneous Symbols and Pictographs block
    (set-fontset-font "fontset-default"
                      '(#x1F300 . #x1F5FF)
                      emojifont)
    ;; Supplemental Symbols and Pictographs block
    (set-fontset-font "fontset-default"
                      '(#x1F900 . #x1F9FF)
                      emojifont)
    ;; Emoticons block
    (set-fontset-font "fontset-default"
                      '(#x1F600 . #x1F64F)
                      emojifont)
    ;; Transport and Map Symbols block
    (set-fontset-font "fontset-default"
                      '(#x1F680 . #x1F6FF)
                      emojifont)
    ;; Miscellaneous Symbols block
    (set-fontset-font "fontset-default"
                      '(#x2600 . #x26FF)
                      emojifont)
    ;; Dingbats block
    (set-fontset-font "fontset-default"
                      '(#x2700 . #x27FF)
                      emojifont)))
