;;; eiffel-mode.el --- major mode for editing Eiffel files.

;; Copyright (C) 1989, 1990, 93, 94, 95, 96, 99, 2000, 01, 02, 04
;;                         Tower Technology Corporation,
;;                         Free Software Foundation,
;;                         Bob Weiner,
;;                         C. Adrian

;; Authors: 1989-1990 Stephen Omohundro, ISE and Bob Weiner
;;          1993-1996 Tower Technology Corporation
;;          1999-2004 Martin Schwenke <martin@meltin.net>
;;          2006-     Berend de Boer <berend@pobox.com>
;; Maintainer: berend@pobox.com
;; Keywords: eiffel languages oop
;; Requires: font-lock, compile, easymenu, imenu

;; This file is derived from eiffel4.el from Tower Technology Corporation.
;;
;; Known bugs:
;;
;; * eiffel-short buffer doesn't get font locked under GNU Emacs 19.34.
;;
;; * eiffel-debug can hang under (at least) XEmacs 21.4.[89] in the wait
;;   loop if there is input pending (that is, if the user hits return
;;   an extra time).  Not yet tested under XEmacs 21.5.
;;
;; This file is distributed under the same terms as GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;; URL: https://github.com/jonhermansen/eiffel-emacs
;; Version: 0.1
;; Keywords: eiffel
;; Package-Requires: ((emacs "24.3"))

;;; Commentary:

;;  NEW VERSIONS
;;    The latest version of this mode is always available via:
;;      http://sourceforge.net/projects/eiffel-emacs/

;;  INSTALLATION
;;    To install, simply copy this file into a directory in your
;;    load-path and add the following two commands in your .emacs file:
;;
;;    (add-to-list 'auto-mode-alist '("\\.e\\'" . eiffel-mode))
;;    (autoload 'eiffel-mode "eiffel" "Major mode for Eiffel programs" t)
;;

;;; History:
;;

;; Add history stuff here!!!
;;
;;   15-July-2009, Roger F. Osmond
;;     Created new group eiffel-attachment-keywords to include new attachment
;;     keywords (attached and detachable).  Assigned builtin face to help
;;     distinguish from other keywords.
;;    Added a number of common class names to eiffel-standard-class-keywords.

;;; Code:

(require 'font-lock)
(require 'compile)
(require 'easymenu)
(require 'imenu)

(defconst eiffel-version-string
  "$Id: eiffel.el,v 2.78 2006/11/15 02:07:56 berenddeboer Exp $"
  "Version string to make reporting bugs more meaningful.
Note that if this file becomes part of GNU Emacs then the file might
be changed by the Emacs maintainers without this version number
changing.  This means that if you are reporting a bug for a version
that was shipped with Emacs, you should report the Emacs version!")

(defgroup eiffel nil
  "Eiffel mode for Emacs"
  :group 'oop)

(defgroup eiffel-indent nil
  "Indentation variables in Eiffel mode"
  :prefix "eiffel-"
  :group 'eiffel)

(defgroup eiffel-compile nil
  "Compilation support variables in Eiffel mode"
  :prefix "eiffel-"
  :group 'eiffel)

(defun eiffel-customize ()
  "Run \\[customize-group] for the `eiffel' group."
  (interactive)
  (customize-group 'eiffel))

;; Indentation amount variables.
;;
;; The default values correspond to style used in ``Eiffel: The
;; Language''.

(defcustom eiffel-indent-increment 3
  "*Default indentation interval (in spaces)."
  :type 'integer
  :group 'eiffel-indent)

(defcustom eiffel-class-level-kw-indent 0
  "*Indentation for Class level keywords.
Specified as number of `eiffel-indent-increments'.  See the variable
`eiffel-class-level-keywords-regexp'."
  :type 'integer
  :group 'eiffel-indent)

(defcustom eiffel-extra-class-level-kw-indent 0
  "*Number of extra spaces to add to `eiffel-class-level-kw-indent'.
This results in the actual indentation of a class level keyword.  Can
be negative."
  :type 'integer
  :group 'eiffel-indent)

(defcustom eiffel-class-level-comment-indent 0
  "*Indentation of comments at the beginning of a class.
Specified as number of `eiffel-indent-increments'."
  :type 'integer
  :group 'eiffel-indent)

(defcustom eiffel-extra-class-level-comment-indent 0
  "*Number of spaces to add to `eiffel-class-level-comment-indent'.
This results in the actual indentation of a class level comment.  Can
be negative."
  :type 'integer
  :group 'eiffel-indent)

(defcustom eiffel-inherit-level-kw-indent 2
  "*Indentation of keywords falling under the Inherit clause.
Specified as number of `eiffel-indent-increments'.  See the variable
`eiffel-inherit-level-keywords'."
  :type 'integer
  :group 'eiffel-indent)

(defcustom eiffel-extra-inherit-level-kw-indent 0
  "*Number of spaces to add to `eiffel-inherit-level-kw-indent'.
This results in the actual indentation of an inherit level keyword.
Can be negative."
  :type 'integer
  :group 'eiffel-indent)

(defcustom eiffel-feature-level-indent 1
  "*Indentation amount of features.
Specified as number of `eiffel-indent-increments'."
  :type 'integer
  :group 'eiffel-indent)

(defcustom eiffel-extra-feature-level-indent 0
  "*Number of spaces to add to `eiffel-feature-level-indent'.
This results in the indentation of a feature.  Can be negative."
  :type 'integer
  :group 'eiffel-indent)

(defcustom eiffel-feature-level-kw-indent 2
  "*Indentation of keywords belonging to individual features.
Specified as number of `eiffel-indent-increments'.  See the variable
`eiffel-feature-level-keywords-regexp'."
  :type 'integer
  :group 'eiffel-indent)

(defcustom eiffel-extra-feature-level-kw-indent 0
  "*Number of spaces to add to `eiffel-feature-level-kw-indent'.
This results in the actual indentation of a feature level keyword.
Can be negative."
  :type 'integer
  :group 'eiffel-indent)

(defcustom eiffel-feature-level-comment-indent 3
  "*Indentation of comments at the beginning of a feature.
Specified as number of `eiffel-indent-increments'."
  :type 'integer
  :group 'eiffel-indent)

(defcustom eiffel-extra-feature-level-comment-indent 0
  "*Number of spaces to add to `eiffel-feature-level-comment-indent'.
This results in the actual indentation of a feature level comment.
Can be negative."
  :type 'integer
  :group 'eiffel-indent)

(defcustom eiffel-body-comment-indent 0
  "*Indentation of comments in the body of a routine.
Specified as number of `eiffel-indent-increments')"
  :type 'integer
  :group 'eiffel-indent)

(defcustom eiffel-extra-body-comment-indent 0
  "*Number of spaces to add to `eiffel-body-comment-indent'.
This results in the actual indentation of a routine body comment.  Can
be negative."
  :type 'integer
  :group 'eiffel-indent)

(defcustom eiffel-check-keyword-indent 0
  "*Extra indentation for the check clause as described in ETL.
Specified as number of `eiffel-indent-increments'.  Default is 0, which
is different than in ETL's 1."
  :type 'integer
  :group 'eiffel-indent)

(defcustom eiffel-extra-check-keyword-indent 0
  "*Number of spaces to add to `eiffel-check-keyword-indent'.
This results in the actual indentation of a check keyword.  Can be
negative."
  :type 'integer
  :group 'eiffel-indent)

(defcustom eiffel-rescue-keyword-indent -1
  "*Extra indentation for the rescue clause as described in ETL.
Specified as number of `eiffel-indent-increments'.  Default is -1."
  :type 'integer
  :group 'eiffel-indent)

(defcustom eiffel-extra-rescue-keyword-indent 0
  "*Number of spaces to add to `eiffel-rescue-keyword-indent'.
This results in the actual indentation of a rescue keyword.  Can be
negative."
  :type 'integer
  :group 'eiffel-indent)

(defcustom eiffel-then-indent 0
  "*Indentation for a `then' appearing on a line by itself.
This is as opposed to a `then' on the same line as an `if'.  Specified
as number of `eiffel-indent-increments'."
  :type 'integer
  :group 'eiffel-indent)

(defcustom eiffel-extra-then-indent 1
  "*Number of spaces to add to `eiffel-then-indent'.
This results in the actual indentation of a `then' appearing on a line
by itself.  Can be negative."
  :type 'integer
  :group 'eiffel-indent)

(defcustom eiffel-continuation-indent 1
  "*Extra indentation for a continued statement line.
Specified as number of `eiffel-indent-increments'."
  :type 'integer
  :group 'eiffel-indent)

(defcustom eiffel-extra-continuation-indent 0
  "*Number of spaces to add to `eiffel-continuation-indent'.
This results in the actual indentation of a continued statement
line.  Can be negative."
  :type 'integer
  :group 'eiffel-indent)

(defcustom eiffel-string-continuation-indent 0
  "*Extra indentation for a continued string.
Specified as number of `eiffel-indent-increments'."
  :type 'integer
  :group 'eiffel-indent)

(defcustom eiffel-extra-string-continuation-indent -1
  "*Number of spaces to add to `eiffel-string-continuation-indent'.
This results in the actual indentation of a continued string.  Can be
negative."
  :type 'integer
  :group 'eiffel-indent)

(defcustom eiffel-indent-string-continuations-relatively-flag t
  "*Non-nil means string continuations are indented relative to 1st character.
That is, `eiffel-string-continuation-indent' and
`eiffel-extra-string-continuation-indent' are added to position of first
character of string.  If nil, string continuations are indented
relative to indent of previous line."
  :type 'boolean
  :group 'eiffel-indent)

(defcustom eiffel-multi-line-string-indent t
  "*Set to nil if multi line strings need indentation or need to be left alone when indenting."
  :type 'boolean
  :group 'eiffel-indent)

(defcustom eiffel-set-tab-width-flag t
  "*Non-nil means `tab-width' is set to `eiffel-indent-increment' in `eiffel-mode'."
  :type 'boolean
  :group 'eiffel-indent)

(defcustom eiffel-preprocessor-indent 0
  "*Indentation for lines GOBO preprocessor directives.
Specified as number of `eiffel-indent-increments' from left margin."
  :type 'integer
  :group 'eiffel-indent)

(defcustom eiffel-fill-max-save 4096
  "*Maximum size of a paragraph to save before filling.
Normally \\[eiffel-fill-paragraph] will mark a buffer as modified even if
the fill operation does not make any changes.  If the paragraph being
filled is smaller than the value of this variable then the contents of
the paragraph will be saved for comparison with the paragraph after
the fill operation.  If they are the same, the buffer modification
state is restored.  Set this to 0 to disable this feature, or a very
big number to enable it for all paragraphs."
  :type 'integer
  :group 'eiffel-indent)

(defcustom eiffel-use-gnu-eiffel t
  "*If t include support for compilation using GNU SmartEiffel."
  :type 'boolean
  :group 'eiffel-compile)

(defcustom eiffel-compile-command
  (if (file-executable-p "/usr/bin/se-compile")
    "se-compile"
  "compile")
  "*Program to use for compiling Eiffel programs.
The default is \"compile\", unless \"/usr/bin/se-compile\" exists, as
in Debian GNU/Linux, when the default value is \"se-compile\"."
  :type 'string
  :group 'eiffel-compile)

(defcustom eiffel-short-command "short"
  "*Program to use for producing short form of Eiffel classes."
  :type 'string
  :group 'eiffel-compile)

(defcustom eiffel-compile-options ""
  "*Options to use for compiling Eiffel programs."
  :type 'string
  :group 'eiffel-compile)

;;
;; No user-customizable definitions below this point.
;;

;;
;; Indentation macros.
;;

(defmacro eiffel-class-level-kw-indent-m ()
  "Indentation amount for Class level keywords (in number of spaces)."
  '(+ (* eiffel-class-level-kw-indent eiffel-indent-increment)
    eiffel-extra-class-level-kw-indent))

(defmacro eiffel-class-level-comment-indent-m ()
  "Indentation amount for Class level comments (in number of spaces)."
  '(+ (* eiffel-class-level-comment-indent eiffel-indent-increment)
    eiffel-extra-class-level-comment-indent))

(defmacro eiffel-inherit-level-kw-indent-m ()
  "Indentation amount for Inherit level keywords (in number of spaces)."
  '(+ (* eiffel-inherit-level-kw-indent eiffel-indent-increment)
    eiffel-extra-inherit-level-kw-indent))

(defmacro eiffel-feature-level-indent-m ()
  "Indentation amount for features (in number of spaces)."
  '(+ (* eiffel-feature-level-indent eiffel-indent-increment)
    eiffel-extra-feature-level-indent))

(defmacro eiffel-feature-level-kw-indent-m ()
  "Indentation amount for Feature level keywords (in number of spaces)."
  '(+ (* eiffel-feature-level-kw-indent eiffel-indent-increment)
    eiffel-extra-feature-level-kw-indent))

(defmacro eiffel-body-comment-indent-m ()
  "Indentation amount for comments in routine bodies (in number of spaces)."
  '(+ (* eiffel-body-comment-indent eiffel-indent-increment)
    eiffel-extra-body-comment-indent))

(defmacro eiffel-feature-level-comment-indent-m ()
  "Indentation amount for Feature level comments (in number of spaces)."
  '(+ (* eiffel-feature-level-comment-indent eiffel-indent-increment)
    eiffel-extra-feature-level-comment-indent))

(defmacro eiffel-check-keyword-indent-m ()
  "Indentation amount for Check keyword (in number of spaces)."
  '(+ (* eiffel-check-keyword-indent eiffel-indent-increment)
    eiffel-extra-check-keyword-indent))

(defmacro eiffel-rescue-keyword-indent-m ()
  "Indentation amount for Rescue keyword (in number of spaces)."
  '(+ (* eiffel-rescue-keyword-indent eiffel-indent-increment)
    eiffel-extra-rescue-keyword-indent))

(defmacro eiffel-then-indent-m ()
  "Indentation amount for `then' appearing on a line by itself (in number of spaces)."
  '(+ (* eiffel-then-indent eiffel-indent-increment)
    eiffel-extra-then-indent))

(defmacro eiffel-continuation-indent-m ()
  "Indentation amount for a statement continuation line (in number of spaces)."
  '(+ (* eiffel-continuation-indent eiffel-indent-increment)
    eiffel-extra-continuation-indent))

(defmacro eiffel-string-continuation-indent-m ()
  "Indentation amount for a statement continuation line (in number of spaces)."
  '(+ (* eiffel-string-continuation-indent eiffel-indent-increment)
    eiffel-extra-string-continuation-indent))

(defmacro eiffel-preprocessor-indent-m ()
  "Indentation amount for a preprocessor statement (in number of spaces)."
  '(* eiffel-preprocessor-indent eiffel-indent-increment))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;          Keyword Regular Expression Constants.               ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconst eiffel-non-id-char-regexp "\\S_" ;; "[^a-z0-9_]"
  "The characters that are not part of identifiers.")

(defun eiffel-post-word-anchor (regexp)
  "Anchor given REGEXP with end-word delimiter and `eiffel-non-id-char-regexp'."
  (concat "\\(" regexp "\\)\\>" eiffel-non-id-char-regexp))

(defun eiffel-word-anchor (regexp)
  "Anchor given REGEXP with word delimiters and `eiffel-non-id-char-regexp'."
  (concat "\\<" (eiffel-post-word-anchor regexp)))

(defun eiffel-post-anchor (regexp)
  "Anchor given REGEXP at end to match line break or non-symbol char."
  (concat "\\(" regexp "\\)\\($\\|\\>\\S_\\)"))

(defun eiffel-anchor (regexp)
  "Anchor given REGEXP front and back to match line break or non-symbol char."
  (concat "\\(^\\|\\S_\\<\\)" (eiffel-post-anchor regexp)))

;; Note invariant is handled as a special case since it is both a
;; class-level and a from-level keyword
;; Note obsolete is handled as a special case since it is both a
;; class-level and a feature-level keyword
(defconst eiffel-class-level-keywords-regexp
  (eiffel-post-anchor
   (concat
  "class\\|feature\\|convert" "\\|"
  "deferred[ \t]+class\\|expanded[ \t]+class" "\\|"
  "reference[ \t]+class\\|separate[ \t]+class" "\\|"
  "inherit\\|creation"))
  "Regexp of keywords introducing class level clauses, with some context.
Note that `invariant' and `obsolete' are not included here since can
function as more than one type of keyword.")

(defconst eiffel-inherit-level-keywords
  "rename\\|redefine\\|undefine\\|select\\|export"
  "Those keywords that introduce subclauses of the inherit clause.")

(defconst eiffel-feature-level-keywords
  "require\\|local\\|deferred\\|separate\\|do\\|once\\|ensure\\|alias\\|external"
  "Those keywords that are internal to features (in particular, routines).")

(defconst eiffel-feature-level-keywords-regexp
  (eiffel-word-anchor eiffel-feature-level-keywords)
  "Regexp of keywords internal to features (usually routines).
See `eiffel-feature-level-keywords'.")

(defconst eiffel-end-keyword "end" "The `end' keyword.")

(defconst eiffel-end-on-current-line ".*[ \t]end[ \t]*;?[ \t]*\\(--.*\\)?$"
  "Regular expression to identify lines ending with the `end' keyword.")

(defconst eiffel-control-flow-keywords
  "if\\|inspect\\|from\\|debug"
  "Keywords that introduce control-flow constructs.")

;RFO
(defconst eiffel-control-flow-matching-keywords
  (concat "deferred\\|do\\|once\\|debug" "\\|" eiffel-control-flow-keywords)
  "Keywords that may cause the indentation of an `eiffel-control-flow-keyword'.
If these occur prior to an `eiffel-control-flow-keyword' then the
`eiffel-control-flow-keyword' is indented.  Note that technically, `end'
is part of this list but it is handled separately in the function
\[eiffel-matching-kw\].")

(defconst eiffel-control-flow-matching-keywords-regexp
  (eiffel-word-anchor eiffel-control-flow-matching-keywords)
  "Regexp of keywords maybe causing indentation of `eiffel-control-flow-keyword'.
See `eiffel-control-flow-keywords'.")

(defconst eiffel-check-keyword "check"
  "The `check' keyword.")

(defconst eiffel-check-keywords-regexp
  (eiffel-word-anchor eiffel-check-keyword)
  "The `check' keyword (with trailing context).")

;; FIXME: Doesn't work if once keyword is followed by a string on next
;; line, but didn't get broken by this attempt at factoring.
(defconst eiffel-check-matching-keywords-regexp
  eiffel-control-flow-matching-keywords-regexp
  "Keywords that may cause the indentation of an `eiffel-check-keyword'.
If these occur prior to an `eiffel-check-keyword' then the
`eiffel-check-keyword' is indented.  Note that technically, `end' is part
of this list but it is handled separately in the function
\[eiffel-matching-kw\].  See also `eiffel-control-flow-matching-keywords-regexp'.")

;; FIXME: This could be fixed or removed.
(defconst eiffel-end-keyword-regexp "\\(^\\|[^a-z0-9_]\\)end\\($\\|[^a-z0-9_]\\)"
  "The `end' keyword with context.")

(defconst eiffel-end-matching-keywords
  (concat "check\\|class\\|feature\\|rename\\|redefine\\|undefine" "\\|"
    "select\\|export\\|separate\\|external\\|alias" "\\|"
    eiffel-control-flow-matching-keywords)
  "Those keywords whose clause is terminated by an `end' keyword.")

(defconst eiffel-end-matching-keywords-regexp
  (eiffel-word-anchor eiffel-end-matching-keywords)
  "Regexp of keywords whose clause is terminated by an `end' keyword.
See `eiffel-end-matching-keywords'.")

(defconst eiffel-rescue-keyword "rescue"  "The `rescue' keyword.")

(defconst eiffel-obsolete-keyword "obsolete"  "The `obsolete' keyword.")

(defconst eiffel-rescue-keywords-regexp
  (eiffel-word-anchor eiffel-rescue-keyword)
  "The `rescue' keyword (with trailing context).")

(defconst eiffel-rescue-matching-keywords-regexp
  (eiffel-word-anchor "deferred\\|do\\|once")
  "Keywords that may cause the indentation of an `eiffel-rescue-keyword'.
If these occur prior to an `eiffel-rescue-keyword' then the
`eiffel-rescue-keyword' is indented.  Note that technically, `end' is
part of this list but it is handled separately in the function
\[eiffel-matching-kw\].  See also `eiffel-control-flow-matching-keywords-regexp'.")

(defconst eiffel-from-level-keywords
  "until\\|variant\\|loop"
  "Keywords occuring inside of a from clause.")

(defconst eiffel-from-level-keywords-regexp
  (eiffel-word-anchor eiffel-from-level-keywords)
  "Regexp of keywords occuring inside of a from clause.
See `eiffel-from-level-keywords'.")

(defconst eiffel-from-keyword  "from" "The keyword `from'.")

(defconst eiffel-if-or-inspect-level-keywords
  "elseif\\|else\\|when"
  "Keywords occuring inside of an if or inspect clause.")

(defconst eiffel-if-or-inspect-level-keywords-regexp
  (eiffel-word-anchor eiffel-if-or-inspect-level-keywords)
  "Regexp of keywords occuring inside of an if or inspect clause.
See eiffel-if-or-inspect-level-keywords.")

(defconst eiffel-if-or-inspect-keyword-regexp
  (eiffel-word-anchor "if\\|inspect")
  "Regexp matching the `if' or `inspect' keywords.")

(defconst eiffel-then-keyword ".*[ \t)]then[ \t]*$"
  "The keyword `then' with possible leading text.")

(defconst eiffel-solitary-then-keyword "then" "The keyword `then'.")

(defconst eiffel-then-matching-keywords "\\(if\\|elseif\\|when\\)"
  "Keywords that may alter the indentation of an `eiffel-then-keyword'.
If one of these occur prior to an `eiffel-then-keyword' then this sets
the indentation of the `eiffel-then-keyword'.  Note that technically,
`end' is part of this list but it is handled separately in the
function \[eiffel-matching-kw\].  See also
`eiffel-control-flow-matching-keywords-regexp'.")

(defconst eiffel-invariant-keyword "invariant" "The `invariant' keyword.")

(defconst eiffel-invariant-matching-keywords
  "from\\|feature"
  "Keywords that may cause the indentation of an `eiffel-invarient-keyword'.
If one of these occurs prior to an `eiffel-invariant-keyword' then the
`eiffel-invariant-keyword' is indented.  Note that technically, `end' is
part of this list but it is handled separately in the function
\[eiffel-matching-kw\].  See also `eiffel-control-flow-matching-keywords-regexp'.")

(defconst eiffel-obsolete-matching-keywords
  "\\(is\\|class\\)"
  "Keywords that may cause the indentation of an `eiffel-obsolete-keyword'.
If one of these occurs prior to an `eiffel-obsolete-keyword' then the
`eiffel-obsolete-keyword' is indented.")

(defconst eiffel-create-keyword
  "create"
  "Eiffel `create' keyword.  Can be used at class or minor level.")

(defconst eiffel-create-keyword-regexp
  (eiffel-post-word-anchor eiffel-create-keyword)
  "Regexp matching `create' keyword, with trailing context.")

(defconst eiffel-indexing-obs-keyword
  "indexing"
  "Eiffel `indexing' keyword, replaced by 'note'.")

(defconst eiffel-indexing-keyword
  "note"
  "Eiffel `note' keyword.  Can be used at class or minor level.")

(defconst eiffel-indexing-keyword-regexp
  (eiffel-post-word-anchor "note\\|indexing")
;;  (eiffel-post-word-anchor eiffel-indexing-keyword)
  "Regexp matching `indexing' or `note' keywords, with trailing context.")

(defconst eiffel-indentation-keywords
  (concat "indexing\\|note\\|convert\\|rescue\\|inherit\\|create" "\\|"
    "invariant\\|require\\|local\\|ensure\\|obsolete" "\\|"
    eiffel-from-level-keywords "\\|"
    eiffel-if-or-inspect-level-keywords "\\|"
    eiffel-end-matching-keywords)
  "Keywords that match any eiffel keyword triggering indentation.")

(defconst eiffel-indentation-keywords-regexp
  (eiffel-word-anchor eiffel-indentation-keywords)
  "Regexp of keywords that match any eiffel keyword triggering indentation.
See `eiffel-indentation-keywords'.")

(defconst eiffel-non-indenting-keywords-regexp
  (concat "\\("
    "once\\(\\s-\\|\n\\)+\"" "\\|"
    (concat (eiffel-post-anchor "feature") ".*\\..*$")
    "\\)")
  "Regexp of keywords with context cancelling any effect on indentation.")

(defconst eiffel-feature-indentation-keywords-regexp
  (eiffel-word-anchor "creation\\|feature")
  "Keywords that denote the presence of features following them.")

;; (defconst eiffel-is-keyword-regexp "\\(.*[ \t)]\\)?is[ \t]*\\(--.*\\)?$"
;;  "The `is' keyword (with some context).")

(defconst eiffel-multiline-routine-is-keyword-regexp
  ".*([^)]*)\\([ \t\n]*\\|[ \t\n]*:[][ \t\nA-Za-x0-9_,]*\\)is[ \t]*\\(--.*\\)?$"
  "The `is' keyword (with some context).")

(defconst eiffel-operator-keywords
  "and\\|or\\|implies"
  "Eiffel operator keywords.")

(defconst eiffel-operator-regexp
  (concat "[ \t]*\\([@*/+]\\|-[^-]\\|\\<\\("
    eiffel-operator-keywords
    "\\)[ \t(]\\)")
  "Eiffel operators - used to identify continuation lines.
See `eiffel-operator-keywords'.")

(defconst eiffel-operator-eol-regexp
  (concat ".*\\([@*/+-]\\|\\<\\(" eiffel-operator-keywords
    "\\)\\|:=\\)[ \t]*\\(--.*\\)?$")
  "Eiffel operators - used to identify continuation lines.")

(defconst eiffel-misc-keywords
  (concat "agent\\|all\\|as\\|frozen\\|infix\\|like" "\\|"
    "old\\|precursor\\|prefix\\|retry\\|strip\\|unique\\|xor" "\\|"
    "expanded\\|reference")
  "Eiffel miscellaneous keywords.")

(defconst eiffel-attachment-keywords
  (concat "attached\\|detachable")
  "Eiffel attachment keywords.")

(defconst eiffel-indexing-keywords
  (concat "indexing\\|note")
  "Eiffel indexing keywords.")

(defconst eiffel-preprocessor-keywords
  "#\\(define\\|undefine\\|ifdef\\|else\\|endif\\|ifndef\\|include\\)"
  "Eiffel GOBO preprocessor keywords.")

(defconst eiffel-preprocessor-keywords-regexp
  (eiffel-post-word-anchor eiffel-preprocessor-keywords)
  "Eiffel GOBO preprocessor keywords, with context.
See `eiffel-preprocessor-keywords'.")

(defconst eiffel-smarteiffel-guru-keywords
  (concat "c_inline_c\\|c_inline_h\\|to_pointer" "\\|"
    "is_expanded_type\\|is_basic_expanded_type" "\\|"
    "object_size\\|object_id_memory" "\\|"
    "se_guru01\\|se_guru02\\|se_guru03")
  "Eiffel keywords used by gurus with the SmartEiffel compiler.")

(defconst eiffel-major-variable-keywords
  (concat "[Vv]oid\\|[Rr]esult\\|[Cc]urrent\\|[Tt]rue\\|[Ff]alse" "\\|"
    "[Pp]recursor\\|io\\|std_input\\|std_output\\|std_error")
  "Eiffel keywords representing major variables.")

(defconst eiffel-standard-class-keywords
  (concat "ANY\\|BIT\\|BOOLEAN\\|CHARACTER\\|DOUBLE\\|GENERAL" "\\|"
    "INTEGER\\|NONE\\|POINTER\\|REAL\\|STRING\\|"
    "STRING_8\\|STRING_32\\|STRING_GENERAL\\|"
    "CHARACTER_8\\|CHARACTER_32\\|"
    "CHARACTER_8_REF\\|CHARACTER_32_REF\\|"
    "INTEGER_8\\|INTEGER_16\\|INTEGER_32\\|INTEGER_64\\|"
    "NATURAL_8\\|NATURAL_16\\|NATURAL_32\\|NATURAL_64\\|"
    "INTEGER_REF\\|BOOLEAN_REF\\|CHARACTER_REF\\|CELL\\|"
    "INTEGER_8_REF\\|INTEGER_16_REF\\|INTEGER_32_REF\\|INTEGER_64_REF\\|"
    "NATURAL_8_REF\\|NATURAL_16_REF\\|NATURAL_32_REF\\|NATURAL_64_REF\\|"
    "REAL_32\\|REAL_64\\|"
    "REAL_32_REF\\|REAL_64_REF\\|"
    "POINTER\\|POINTER_REF\\|TUPLE\\|TYPE\\|"
    "ARRAY\\|HASH_TABLE\\|LINKED_LIST\\|TWO_WAY_LIST"
    )
  "Eiffel keywords representing standard classes.")

(defconst eiffel-all-keywords
  (concat eiffel-indentation-keywords    "\\|"
    eiffel-solitary-then-keyword   "\\|"
    eiffel-create-keyword          "\\|"
    eiffel-end-keyword)
  "Regexp matching (nearly) any eiffel keyword in a line.
Does not include `is'.")

(defconst eiffel-all-keywords-regexp
  (concat "\\("
    (eiffel-word-anchor eiffel-all-keywords) "\\|"
    eiffel-preprocessor-keywords-regexp   "\\)")
  "Anchored regexp matching (nearly) any eiffel keyword in a line.
Does not include `is'.  See `eiffel-all-keywords'.")

(defconst eiffel-comment-start-skip
  "--+|?[ \t]*"
  "Regexp matching the beginning of an Eiffel comment.")

(defconst eiffel-non-source-line
  (concat "[ \t]*\\(\\(" "--" "\\|"
    eiffel-preprocessor-keywords-regexp "\\).*\\)?$")
  "RE matching line with only whitespace and comment or preprocessor keyword.")

;; Factor out some important important regexps for use in
;; eiffel-{beginning,end}-of-feature.

(defconst eiffel-routine-begin-regexp
  "\\([a-z_][a-zA-Z_0-9]*\\)\\s-*\\(([^)]*)\\)?\\s-*\\(:\\s-*[A-Z]\\([][,A-Za-z0-9_]\\|\\s-\\)*\\)?\\s-*\\<is\\>\\s-*\\(--.*\\)?$"
  "Regexp matching the beginning of an Eiffel routine declaration.")

(defconst eiffel-attribute-regexp
  (concat "[a-z_][^-:\n]*:\\s-*"
    "\\(like\\s-*[a-zA-Z][a-z_0-9]*\\|"
    "\\(\\(expanded\\|reference\\)\\s-*\\)?[A-Z][A-Z_0-9]*"
    "\\(\\s-*\\[[^-\n]*\\]\\)?\\)"
    "\\s-*\\($\\|[;)].*\\|--.*\\)")
  "Regexp matching an Eiffel attribute, parameter or local variable.")

(defconst eiffel-constant-regexp
  "[a-z_][^-:\n]*:[^-\n]*\\s-\\<is\\>\\s-+[^ \t\n]"
  "Regexp matching an Eiffel constant declaration.")

(defconst eiffel-variable-or-const-regexp
  (concat eiffel-attribute-regexp "\\|"
    eiffel-constant-regexp)
  "RE to match a variable or constant declaration.")

(defconst eiffel-id-colon-regexp
  "[ \t]*[a-zA-Z0-9_]+[ \t]*:"
  "Regexp that matches an Eiffel assertion tag expression.")

(defconst eiffel-probably-feature-regexp
  (concat "\\(" eiffel-routine-begin-regexp
    "\\|" eiffel-attribute-regexp
    "\\|" eiffel-constant-regexp "\\)")
  "Regexp probably matching an Eiffel feature.
This will also match local variable and parameter declarations.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar eiffel-matching-indent -1
  "Indentation of the keyword found on the last call to \[eiffel-matching-kw\].
-1 if no match was found.")

(defvar eiffel-matching-kw-for-end nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;
;; Font-lock support.
;;
;; Most of this font-lock code was originally contributed by: Karl
;; Landström <kala9469@student.su.se>.  Much of it has now been
;; factored out above.
;;

(defconst eiffel-font-lock-keywords-1
  `(;; hidden comments
  ("--|.*" 0 font-lock-keyword-face t)
  ;; routines
  (,(concat "^[ \t]*" eiffel-routine-begin-regexp) 1 font-lock-function-name-face ))
  "Regular expressions to use with font-lock mode.")

(defconst eiffel-font-lock-keywords-2
  (append
   eiffel-font-lock-keywords-1
   `(;; Assertions.
   ;; FIXME: Cyril thinks these should just be part of the keywords below.
   (,(eiffel-anchor "check\\|ensure then\\|ensure\\|invariant\\|require else\\|require\\|variant") 2 font-lock-reference-face nil)

   ;; Preprocessor keywords.  Note that, by luck more than planning,
   ;; these aren't font-locked when they're not indented, since the
   ;; '#' isn't a word boundary (which is added by eiffel-anchor).
   (,(eiffel-post-word-anchor eiffel-preprocessor-keywords) 2 font-lock-builtin-face nil)

   ;; Keywords.  The first few can appear in conjunction with other
   ;; keywords, and the anchored regexp doesn't cater for overlaps,
   ;; thus there are several entries here.
   (,(eiffel-anchor "class\\|is\\|not")        2 font-lock-keyword-face nil)
   (,(eiffel-anchor eiffel-operator-keywords)     2 font-lock-keyword-face nil)
   (,(eiffel-anchor eiffel-misc-keywords)         2 font-lock-keyword-face nil)
   (,(eiffel-anchor eiffel-attachment-keywords)   2 font-lock-builtin-face nil)
   (,(eiffel-anchor eiffel-all-keywords)          2 font-lock-keyword-face nil)

   ;; Quoted expr's in comments.
   ("`[^`'\n]*'" 0 font-lock-string-face t)

   ;; Classes.
   (,(eiffel-anchor eiffel-standard-class-keywords) 2 font-lock-type-face)))
   "Regular expressions to use with font-lock mode and level 2 fontification.")

(defconst eiffel-font-lock-keywords-3
  (append
   eiffel-font-lock-keywords-2
   `(;; attributes/parameters/local variables
   (,(concat "^[ \t]*" eiffel-attribute-regexp) (0 nil)
    ("\\s-*\\(\\<[a-z][a-zA-Z_0-9]*\\)\\s-*\\(,\\|:[^;\n]*\\|).*\\)"
     (re-search-backward "\\((\\|^\\)" nil t)
     (end-of-line)
     (1 font-lock-variable-name-face)))
   ;; constants
   (,(concat "^[ \t]*" eiffel-constant-regexp) (0 nil)
    ("\\s-*\\(\\<[A-Za-z][a-zA-Z_0-9]*\\)\\s-*\\(,\\|:.*\\)"
     (beginning-of-line) (end-of-line)
     (1 font-lock-constant-face)))
   (,(concat "^[ \t]*" eiffel-id-colon-regexp "\\($\\|[^=]\\)") (0 nil)
    ("\\s-*\\(\\<[A-Za-z][a-zA-Z_0-9]*\\)\\s-*:"
     (beginning-of-line) (end-of-line)
     (1 font-lock-constant-face)))
))
  "Regular expressions to use with font-lock mode and level 3 fontification.")

(defconst eiffel-font-lock-keywords-4
  ;; SmartEiffel guru keywords and major variables.
  (append
   eiffel-font-lock-keywords-3
   `((,(eiffel-anchor eiffel-smarteiffel-guru-keywords) 2 font-lock-warning-face)
   (,(eiffel-anchor eiffel-major-variable-keywords) 2 font-lock-constant-face))))

(defvar eiffel-font-lock-keywords eiffel-font-lock-keywords-1
  "Default expressions to highlight in Eiffel mode.
See also `c-font-lock-extra-types'.")

(defconst eiffel-font-lock-defaults
  '((eiffel-font-lock-keywords
   eiffel-font-lock-keywords-1
   eiffel-font-lock-keywords-2
   eiffel-font-lock-keywords-3
   eiffel-font-lock-keywords-4)
  nil nil nil nil))

(and (boundp 'font-lock-defaults-alist)
   (add-to-list 'font-lock-defaults-alist
      (cons 'eiffel-mode
      eiffel-font-lock-defaults)))

;; font-lock faces used by GNU Emacs and XEmacs are inconsistent.
(if (and (not (boundp 'font-lock-constant-face))
   (fboundp 'copy-face))
  (copy-face 'font-lock-variable-name-face 'font-lock-constant-face))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;
;; Compilation support for GNU SmartEiffel.
;;

(defvar eiffel-compile-dir nil
  "Current directory where Eiffel compilations are taking place.
Possibly used for error location.")

(defvar eiffel-ace-file nil
  "Current Eiffel ace file being compiled/debugged.")

(defvar eiffel-root-class nil
  "Current Eiffel root class being compiled/debugged.")

(defvar eiffel-compile-target nil
  "Current Eiffel compilation target.")

(defvar eiffel-debug-target nil
  "Current Eiffel debug target.")

(defvar eiffel-root-proc "make"
  "Current Eiffel root procedure.")

(defvar eiffel-run-command nil
  "Current command to run after Eiffel compile.")

(defvar eiffel-debug-command nil
  "Current debug command to run after Eiffel debug compile.")

(defun eiffel-compilation-mode-hook ()
  "Hook function to set local value for `compilation-error-screen-columns'.
This should be nil for SmartEiffel compiles, because column positions are
returned as character positions rather than screen columns."
  ;; In Emacs > 20.7 compilation-error-screen-columns is buffer local.
  (or (assq 'compilation-error-screen-columns (buffer-local-variables))
    (make-local-variable 'compilation-error-screen-columns))
  (setq compilation-error-screen-columns nil))

(defun eiffel-compile ()
  "Compile an Eiffel root class."
  (interactive)
  (eiffel-compile-prompt)
  (eiffel-compile-internal))

(defun eiffel-set-compile-options ()
  "Set Eiffel compiler options."
  (interactive)
  (setq eiffel-compile-options
  (read-string "Eiffel compiler options: " eiffel-compile-options)))

;; Taken from Emacs 20.3 subr.el (just in case we're running under Emacs 19).
(defun eiffel-split-string (string &optional separators)
  "Split STRING into substrings separated by SEPARATORS.
Each match for SEPARATORS is a splitting point.  The substrings
between the splitting points are made into a list which is returned.
If SEPARATORS is absent, it defaults to \"[ \\f\\t\\n\\r\\v]+\".

If there is match for SEPARATORS at the beginning of STRING, we do not
include a null substring for that.  Likewise, if there is a match
at the end of STRING, we do not include a null substring for that."
  (let ((rexp (or separators "[ \f\t\n\r\v]+"))
  (start 0)
  notfirst
  (list nil))
  (while (and (string-match rexp string
          (if (and notfirst
             (= start (match-beginning 0))
             (< start (length string)))
          (1+ start) start))
    (< (match-beginning 0) (length string)))
    (setq notfirst t)
    (or (eq (match-beginning 0) 0)
    (and (eq (match-beginning 0) (match-end 0))
       (eq (match-beginning 0) start))
    (setq list
    (cons (substring string start (match-beginning 0))
        list)))
    (setq start (match-end 0)))
  (or (eq start (length string))
  (setq list
      (cons (substring string start)
      list)))
  (nreverse list)))

(defun eiffel-run ()
  "Run a compiled Eiffel program."
  (interactive)
  (setq eiffel-run-command
  (read-string "Command to run: "
       (or eiffel-run-command
       eiffel-compile-target
       (file-name-sans-extension
        (if (eq system-type 'windows-nt)
          buffer-file-name
        (file-name-nondirectory (buffer-file-name)))))))
  (eiffel-run-internal))

(defun eiffel-debug ()
  "Run the SmartEiffel debugger."
  (interactive)

  (eiffel-compile-prompt)

  (setq eiffel-debug-target
  (file-name-sans-extension
   (read-string "Debug target name: "
        (or eiffel-debug-target
        (concat eiffel-compile-target "_debug")))))

  (let* ((eiffel-compile-options (concat "-sedb " eiffel-compile-options))
   (eiffel-compile-target eiffel-debug-target)
   (buff (eiffel-compile-internal))
   (proc (get-buffer-process buff)))

  ;; This works under GNU Emacs, but hangs under at least some
  ;; versions of XEmacs if there is input pending.
  (while (eq (process-status proc) 'run)
    (sit-for 1))

  (if (= (process-exit-status proc) 0)
  (progn
    (setq eiffel-debug-command
    (read-string "Debugger command to run: "
         (or eiffel-debug-command
         eiffel-debug-target
         (file-name-sans-extension
          (if (eq system-type 'windows-nt)
            buffer-file-name
          (file-name-nondirectory
           (buffer-file-name)))))))
    (let ((eiffel-run-command eiffel-debug-command))
    (eiffel-run-internal))))))

(defun eiffel-compile-prompt ()
  "Prompt for information required to compile an Eiffel root class."

  ;; Do the save first, since the user might still have their hand on
  ;; the mouse.
  (save-some-buffers (not compilation-ask-about-save) nil)

  (setq eiffel-compile-dir (file-name-directory (buffer-file-name)))
  (setq eiffel-ace-file
      (read-string "Name of ace file (leave empty to get prompted for root class): " eiffel-ace-file))
  (if (string= eiffel-ace-file "")
    (progn
      (setq eiffel-root-class
        (file-name-sans-extension
         (read-string "Name of root class: "
                (or eiffel-compile-target
                  (file-name-sans-extension
                   (file-name-nondirectory (buffer-file-name)))))))
      (setq eiffel-compile-target eiffel-root-class)
      (setq eiffel-root-proc
        (read-string "Name of root procedure: "
               eiffel-root-proc)))))

(defun eiffel-compile-internal ()
  "Compile an Eiffel root class.  Internal version.
Returns the same thing as \\[compile-internal] - the compilation buffer."

  (let
      ((cmd (concat eiffel-compile-command
                    " "    eiffel-compile-options
                    " "    eiffel-ace-file
                    (if (string= eiffel-ace-file "")
                        (concat "-o " eiffel-compile-target
                                (if (eq system-type 'windows-nt) ".exe")
                                " "    eiffel-root-class
                                " "    eiffel-root-proc))))
       (compilation-mode-hook (cons 'eiffel-compilation-mode-hook
                                    compilation-mode-hook)))
    (compilation-start cmd "No more errors")))

(defun eiffel-run-internal ()
  "Run a compiled Eiffel program.  Internal version."

  (let* ((tmp-buf (current-buffer))
   (words   (eiffel-split-string eiffel-run-command))
   (cmd     (expand-file-name (car words))))

  (apply 'make-comint cmd cmd nil (cdr words))
  (switch-to-buffer tmp-buf)
  (switch-to-buffer-other-window (concat "*" cmd "*"))))

;; This has been loosened up to spot parts of messages that contain
;; references to multiple locations.  Thanks to Andreas
;; <nozone@sbox.tu-graz.ac.at>.  Also, the column number is a character
;; count rather than a screen column, so we need to make sure that
;; compilation-error-screen-columns is nil.  Note that in XEmacs this
;; variable doesn't exist, so we end up in the wrong column.  Hey, at
;; least we're on the correct line!
(add-to-list 'compilation-error-regexp-alist
     '("^Line \\([0-9]+\\) column \\([0-9]+\\) in [^ ]+ (\\([^)]+\\.[Ee]\\))" 3 1 2))

(defun eiffel-short ()
  "Display the short form of an Eiffel class."
  (interactive)
  (let* ((class (read-string
     "Class or file: "
     (if (buffer-file-name)
       (file-name-nondirectory (buffer-file-name)))))
   (buf (get-buffer-create (concat "*Eiffel - short " class "*"))))

  (shell-command (concat eiffel-short-command " " class) buf)
  (save-excursion
    (set-buffer buf)
    (let ((font-lock-defaults eiffel-font-lock-defaults))
  (font-lock-fontify-buffer))
    (toggle-read-only 1))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                      Utility Functions.                      ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun eiffel-feature-quote ()
  "Put a `' around the current feature name."
  (interactive)
  (save-excursion
  ;; Only try to go back to the beginning of the feature if we're
  ;; not already there.
  (if (/= (point)
    (save-excursion
      (forward-sexp)
      (backward-sexp)
      (point)))
  (backward-sexp))
  (insert "`")
  (forward-sexp)
  (insert "'"))
  (if (looking-at "'")
    (forward-char 1)))

(defun eiffel-peeking-backwards-at (regexp)
  "Return non-nil is previous character exists and is matched by REGEXP.
The match is actually an unbounded match starting at the previous character."
  (save-excursion
  (save-match-data
    (and (not (bobp))
     (or (backward-char) t)
     (looking-at regexp)))))

(defsubst eiffel-in-comment-p ()
  "Return t if point is in a comment."
  (interactive)
  (save-excursion
  (nth 4 (parse-partial-sexp
    (save-excursion (beginning-of-line) (point))
    (point)))))

(defun eiffel-in-comment-or-quoted-string-p ()
  "Return t if point is in a comment or quoted string."
  (or (eiffel-in-comment-p)
    (eiffel-in-quoted-string-p)))

(defun eiffel-not-in-comment-or-quoted-string-p ()
  "Return t if point is not in a comment or quoted string."
  (not (eiffel-in-comment-or-quoted-string-p)))

(defun eiffel-near-comment-p ()
  "Return t if point is close enough to a comment for filling purposes."
  (or (eiffel-in-comment-p)
    (and (or (looking-at comment-start-skip)
       (eiffel-peeking-backwards-at comment-start-skip))
     (not (eiffel-in-quoted-string-p)))
    (looking-at (concat "[ \t]*" comment-start-skip))))

(defun eiffel-re-search-forward (regexp &optional limit noerror)
  "Search forward from point for REGEXP not in comment or string.
`case-fold-search' is set to nil when searching.  For details on other
arguments see \\[re-search-forward]."

  (interactive "sRE search: ")
  (let ((start (point))
  found case-fold-search)
  (while (and (setq found (re-search-forward regexp limit noerror))
    (eiffel-in-comment-or-quoted-string-p)))
  (if (and found
    (eiffel-not-in-comment-or-quoted-string-p))
  found
    (if (eq noerror t)
    (goto-char start))
    nil)))

(defun eiffel-re-search-backward (regexp &optional limit noerror)
  "Search backward from point for REGEXP not in comment or string.
`case-fold-search' is set to nil when searching.  For details on other
arguments see \\[re-search-forward]."
  (interactive "sRE search: ")
  (let ((start (point))
  found case-fold-search)
  (while (and (setq found (re-search-backward regexp limit noerror))
    (eiffel-in-comment-or-quoted-string-p)))
  (if (and found
    (eiffel-not-in-comment-or-quoted-string-p))
  found
    (if (eq noerror t)
    (goto-char start))
    nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                      Indentation Functions.                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun eiffel-skip-leading-whitespace ()
  "OBSOLETE 2006-04-26: please use `back-to-indentation' instead."
  (back-to-indentation))

(defun eiffel-calc-indent ()
  "Calculate the indentation of the current line of eiffel code.
This function handles the case where there is a keyword that affects
indentation at the beginning of the current line.  For lines that
don't start with a relevant keyword, the calculation is handed off to
\\[eiffel-calc-non-keyword-indent]."
  (let ((indent   0)
  kw-match)

  (save-excursion
    (back-to-indentation)

    ;; Look for a keyword on the current line.
    (if (looking-at eiffel-all-keywords-regexp)

    (cond ((or (looking-at eiffel-create-keyword-regexp)
       (looking-at eiffel-indexing-keyword-regexp))
     ;; Class-level or minor occurence?
     (if (save-excursion (eiffel-find-beginning-of-feature))
       ;; Minor.
       (setq indent (eiffel-calc-indent-non-keyword))
       ;; Class-level.
       (setq indent (eiffel-class-level-kw-indent-m))))
    ;; There's possibly a better way of coding this exception.
    ((looking-at eiffel-non-indenting-keywords-regexp)
     (setq indent (eiffel-calc-indent-non-keyword)))
    ((looking-at eiffel-class-level-keywords-regexp)
     ;; File level keywords (indent defaults to 0)
     (setq indent (eiffel-class-level-kw-indent-m)))
    ((looking-at eiffel-inherit-level-keywords)
     ;; Inherit level keywords (indent defaults to
     ;; 2*eiffel-indent-increment)
     (setq indent (eiffel-inherit-level-kw-indent-m)))
    ((looking-at eiffel-feature-level-keywords-regexp)
     ;; Feature level keywords (indent defaults to
     ;; (eiffel-feature-level-indent-m) + eiffel-indent-increment)
     (setq indent (eiffel-feature-level-kw-indent-m)))
    ((looking-at eiffel-end-keyword)
     ;; End keyword (indent to level of matching keyword)
     (if (string-match "end"
           (eiffel-matching-kw
          eiffel-end-matching-keywords-regexp))
       ;; Then
       (if (= eiffel-matching-indent
        (eiffel-feature-level-kw-indent-m))
       ;; Then
       (setq indent (eiffel-class-level-kw-indent-m))
         ;; Else
         (setq indent
         (- eiffel-matching-indent eiffel-indent-increment)))
       ;; Else
       (setq indent eiffel-matching-indent))
     ;; FIXME: This is broken!!!
     (if (<= indent (eiffel-feature-level-indent-m))
       (save-excursion
         (end-of-line)
         (while (and (< (point) (point-max))
           (or (forward-char 1) t)
           (looking-at eiffel-non-source-line))
       (end-of-line))
         (if (not (looking-at eiffel-non-source-line))
         (setq indent (eiffel-inherit-level-kw-indent-m))
       (setq indent (eiffel-class-level-kw-indent-m))))))
    ((looking-at eiffel-control-flow-keywords)
     ;; Control flow keywords
     ;;  Indent to same level as a preceding "end" or
     ;;  if no preceding "end" is found, indent to the level
     ;;  of the preceding "do" plus the value of
     ;;  eiffel-indent-increment
     (setq kw-match
         (eiffel-matching-kw
      eiffel-control-flow-matching-keywords-regexp))
     (cond ((string-match "end" kw-match)
      (setq indent eiffel-matching-indent))
         (t
      (setq indent
          (+ eiffel-matching-indent eiffel-indent-increment)))))
    ((looking-at eiffel-check-keywords-regexp)
     ;; Check keyword
     ;;  Indent to level of preceding "end"+eiffel-indent-increment or
     ;;  if no preceding "end" is found, indent to the level of
     ;;  the preceding eiffel-check-matching-keywords-regexp plus the
     ;;  value (eiffel-indent-increment + eiffel-check-keyword-indent).

     (setq kw-match (eiffel-matching-kw
         eiffel-check-matching-keywords-regexp))
     (cond ((string-match "end" kw-match)
      (setq indent (+ eiffel-matching-indent
          (eiffel-check-keyword-indent-m))))
         (t
      (setq indent
          (+ eiffel-matching-indent
         (+ eiffel-indent-increment
          (eiffel-check-keyword-indent-m)))))))
    ((looking-at eiffel-rescue-keywords-regexp)
     ;; Rescue keyword
     ;;  Indent to level of preceding "end"+eiffel-indent-increment or
     ;;  if no preceding "end" is found, indent to the level of
     ;;  the preceding eiffel-rescue-matching-keywords-regexp plus the
     ;;  value (eiffel-indent-increment + eiffel-rescue-keyword-indent).
     (setq kw-match (eiffel-matching-kw
         eiffel-rescue-matching-keywords-regexp))
     (cond ((string-match "end" kw-match)
      (setq indent (+ eiffel-matching-indent
          (eiffel-rescue-keyword-indent-m))))
         (t
      (setq indent eiffel-matching-indent))))
    ((looking-at eiffel-from-level-keywords-regexp)
     ;; From level keywords (indent to level of matching "From")
     (if (string-match "end" (eiffel-matching-kw eiffel-from-keyword))
       ;; Closest matching KW is `end'.
       (setq indent (- eiffel-matching-indent eiffel-indent-increment))
       ;; Closest matching KW is one of `eiffel-from-keyword'.
       (setq indent eiffel-matching-indent)))
    ((looking-at eiffel-if-or-inspect-level-keywords-regexp)
     ;; If level keywords (indent to level of matching
     ;; "If" or "Inspect")
     (if (string-match "end"
           (eiffel-matching-kw
          eiffel-if-or-inspect-keyword-regexp))
       ;; Closest matching KW is `end'.
       (setq indent (- eiffel-matching-indent eiffel-indent-increment))
       ;; Closest matching KW is one of `eiffel-if-or-inspect-keyword-regexp'.
       (setq indent eiffel-matching-indent)))
    ((looking-at eiffel-solitary-then-keyword)
     ;; Handles case where "then" appears on a line by itself
     ;;   (Indented to level of the matching if, elseif or when)
     (eiffel-matching-kw eiffel-then-matching-keywords)
     (setq indent (+ eiffel-matching-indent (eiffel-then-indent-m))))
    ((looking-at eiffel-invariant-keyword)
     ;; Invariant keyword
     ;;   (Indented to level of the matching from or feature)
     (if (string-match "from"
           (eiffel-matching-kw eiffel-invariant-matching-keywords))
       ;; Then - loop invariant
       (setq indent eiffel-matching-indent)
       ;; Else - class invariant
       (setq indent (eiffel-class-level-kw-indent-m))))
    ((looking-at eiffel-obsolete-keyword)
     ;; Obsolete keyword
     ;;   (Indented to the level of the matching from or feature)
     (if (string-match "is"
           (eiffel-matching-kw eiffel-obsolete-matching-keywords))
       ;; Then - feature obsolete
       (setq indent (eiffel-feature-level-kw-indent-m))
       ;; Else - class obsolete
       (setq indent (eiffel-class-level-kw-indent-m))))
    ((looking-at eiffel-preprocessor-keywords-regexp)
     (setq indent (eiffel-preprocessor-indent-m))))
  ;; No keyword.  Hand off...
  (setq indent (eiffel-calc-indent-non-keyword))))
  indent))

(defun eiffel-calc-indent-non-keyword ()
  "Calculate indentation of current Eiffel code line, without leading
keyword.  This function generally assumes that the preceding line of
code is indented properly, and usually bases the indentation of the
current line on that preceding line. This function assumes
`back-to-indentation' is in effect."
  (let (previous-line-indent what-indentation)
    (save-excursion

      ;; Are we in a multi-line string expression?
      (if (eiffel-in-multiline-string-expression)
          ;; Depending on a setting, we either don't indent, or indent
          ;; just as much as the previous line.
          ;; The latter implies that the first line, immediately below
          ;; the "[ is indented at the same level as the feature name,
          ;; which isn't too bad.
          (if eiffel-multi-line-string-indent
              (if (looking-at "[]]\"")
                  0
                (let (beginning-of-line-position)
                  (save-excursion
                    (end-of-line 0)
                    (backward-char 2)
                    (if (looking-at "\"[[]")
                        0
                      (back-to-indentation)
                      (current-column)))))
            (back-to-indentation) (current-column))
        ;; TODO:
        ;; 2. indentation with arrays?
        (setq previous-line-indent (eiffel-previous-line-indent))
        ;; `eiffel-calc-indent' does not consider certain things a keyword we
        ;; will consider a keyword here. So let's first handle those.
        (cond
         ;;
         ((< previous-line-indent 0) (+ (abs previous-line-indent) eiffel-indent-increment))
         ;; recognise feature level comments
         ((and (looking-at "--") (= previous-line-indent (eiffel-feature-level-indent-m)))
          (eiffel-feature-level-comment-indent-m))
         ;; string continuation, distinguish between the first/second line
         ;; of such a continuation.
         ((looking-at "%")
          (if (eiffel-previous-line-is-string-continuation-line)
              previous-line-indent
            (+ previous-line-indent (eiffel-string-continuation-indent-m))))
         ;; if current line starts with an operator, we have to indent or
         ;; stay at the same indent if the previous line is already a continuation.
         ((looking-at eiffel-operator-keywords)
          (if (or (eiffel-previous-line-is-continuation) (eiffel-previous-previous-line-is-continuation))
              previous-line-indent
            (+ previous-line-indent eiffel-indent-increment)))
         ;; if line starts with closing parenthesis, we match the indent
         ;; of opening parenthesis.
         ((looking-at "\)")
          (forward-char)
          (backward-list)
          (current-column))
         ;; else we have to look at the previous line
         (t
          (setq what-indentation (eiffel-what-indentation))
          (cond
           ((eq what-indentation 'eiffel-what-indent-class-level-comment)
            (eiffel-class-level-comment-indent-m))
           ((eq what-indentation 'eiffel-what-indent-as-previous)
            previous-line-indent)
           ((eq what-indentation 'eiffel-what-indent-increase)
            (+ previous-line-indent eiffel-indent-increment))
           ((eq what-indentation 'eiffel-what-indent-decrease)
            (- previous-line-indent eiffel-indent-increment))
           (what-indentation))))))))

(defun eiffel-what-indentation ()
  "Determine what indentation is required. There are basically three
options: increase the indentation, decrease it, or keep it the same as
the previous line. Besides that there are a few minor cases. This
function assumes `back-to-indentation' is in effect."
  (let (looking-at-comment)
    (save-excursion

      ;; Remember if we were looking at a comment
      (setq looking-at-comment (looking-at "--"))

      (backward-sexp)
      ;; in case where we might be looking at feature {NONE} for example
      ;; go back one sexp to point at feature
      ;; 2006-11-07: formerly also looked at ')' character, removed it.
      (if (looking-at "[{]")
          (backward-sexp))
      (cond
       ;; the end statement is a bit difficult: inside a body the next
       ;; line (our current line) should be indented at the same level
       ;; but the end of feature signals a decrease.
       ((looking-at "end\\([ \t]\\|$\\)")
        (if (= (eiffel-current-line-indent) (eiffel-feature-level-kw-indent-m))
            'eiffel-what-indent-decrease
          'eiffel-what-indent-as-previous))
       ;; indent if previous line starts with these keywords
       ((looking-at "\\(indexing\\|note\\|deferred\\|expanded\\|separate\\|class\\|rename\\|export\\|undefine\\|redefine\\|inherit\\|create\\|feature\\|is\\|obsolete\\|require\\|local\\|do\\|once\\|if\\|inspect\\|when\\|from\\|variant\\|invariant\\|until\\|loop\\|check\\|debug\\|rescue\\|ensure\\|invariant\\)\\([ \t]\\|$\\)") 'eiffel-what-indent-increase)
       ;; then and else must be treated differently, it should not be
       ;; part of the "and then" or "or else" operators.
       ((and (looking-at "then\\([ \t]\\|$\\)") (not (eiffel-is-preceded-by "and")))
        'eiffel-what-indent-increase)
       ((and (looking-at "else\\([ \t]\\|$\\)") (not (eiffel-is-preceded-by "or")))
        'eiffel-what-indent-increase)
       ;; we always indent the next line if the previous line ends
       ;; with "implies"
       ((looking-at "implies\\([ \t]\\|$\\)")
          'eiffel-what-indent-increase)
       ;; determine if we're on a continuation; like a string
       ;; continuation we have to distinguish between the first
       ;; continuation and subsequent continuations.
       ((eiffel-line-ends-with-continuation-symbol)
        (if (and (not (eiffel-line-begins-with-label)) (or (eiffel-current-line-is-continuation) (eiffel-previous-line-is-continuation) (eiffel-is-first-line-after-boolean-keyword)))
            'eiffel-what-indent-as-previous
          'eiffel-what-indent-increase))
       ;; The current line is a continuation if the previous line is a
       ;; continuation. But the line we're asked to indent isn't as
       ;; far as we can tell, because the current line gives no
       ;; indication that the next line is a continuation. In that
       ;; case we have to decrease the indentation back to the first
       ;; line we can find that isn't a continuation.
       ((eiffel-previous-line-is-continuation)
        (eiffel-indent-of-last-non-continuation-line))
       ((= (point) 1)
        (if looking-at-comment
            'eiffel-what-indent-class-level-comment
          'eiffel-what-indent-as-previous))
       (t `eiffel-what-indent-as-previous)))))

(defun eiffel-is-preceded-by (word)
  "Is the previous word equal to word?"
  (save-excursion
    (backward-sexp)
    (looking-at (concat word "\\([ \t]\\|$\\)"))))

(defun eiffel-line-begins-with-label ()
  "Does the current line begin with a label?"
  (save-excursion
    (back-to-indentation)
    (looking-at "[a-zA-Z0-9_]+:")))

(defun eiffel-is-first-line-after-boolean-keyword ()
  "Are we on the first line following the keywords require, ensure,
until or if?"
  (save-excursion
    (beginning-of-line)
    (condition-case nil
        (let () (backward-sexp) (looking-at "\\(require\\|if\\|elseif\\|until\\|ensure\\)\\([ \t]\\|$\\)"))
      (error t))))

(defun eiffel-previous-line-is-string-continuation-line ()
  "Is the previous line a string continuation line?"
  (save-excursion
    (backward-sexp)
    (looking-at "%")))

(defun eiffel-line-ends-with-continuation-symbol ()
  "Does the line end with an operator or colon? Assumes that with a `backward-sexp' we have come unto this line."
  (save-excursion
    (cond
     ((looking-at "\\(and\\|or\\|implies\\)\\([ \t]\\|$\\)") t)
     ((and (looking-at "then\\([ \t]\\|$\\)") (eiffel-is-preceded-by "and")) t)
     ((and (looking-at "else\\([ \t]\\|$\\)") (eiffel-is-preceded-by "or")) t)
     (t
      (forward-sexp)
      (looking-at "[ \t]*\\([@*/+:=]\\|-[^-]\\)")))))

(defun eiffel-current-line-is-continuation ()
  "Is current line a continuation, based upon if it starts with an operator?"
  (save-excursion
    (back-to-indentation)
    (looking-at eiffel-operator-regexp)))

(defun eiffel-previous-line-is-continuation ()
  "Does the previous line indicate that the next line is a continuation?"
  (save-excursion
    (condition-case nil
        (eiffel-previous-line-is-continuation)
      (error nil))))

(defun eiffel-previous-line-is-continuation ()
  "Does the previous line indicate that the next line is a continuation?"
  (save-excursion
    (beginning-of-line)
    (backward-sexp)
    (eiffel-line-ends-with-continuation-symbol)))

(defun eiffel-previous-previous-line-is-continuation ()
  "Is the line before the previous line a continuation?"
  (save-excursion
    (beginning-of-line)
    (backward-sexp)
    (beginning-of-line)
    (backward-sexp)
    (eiffel-line-ends-with-continuation-symbol)))

(defun eiffel-calc-indent-in-multi-line-string ()
  "Indentation if inside a multi-line string."
  ;; If user has just ended the string, the identation is zero.
  (if (looking-at "[]]\"")
      0
    ;; Else for first line just below "[ the indentation is zero.
    ;; For any subsequent line it's the identation of the previous line.
    (save-excursion
      (forward-line -1)
      (end-of-line)
      (backward-char 3)
      (if (looking-at "[^%]\"[[]")
          0
        (back-to-indentation)
        (current-column)))))

(defun eiffel-previous-line-indent ()
  "Amount of identation of previous line."
  (save-excursion
    (condition-case nil
        (eiffel-previous-line-indent)
      (error (eiffel-previous-line-indent2)))))

(defun eiffel-previous-line-indent ()
  "Indentation of previous sexp"
  (backward-sexp)
  (back-to-indentation)
  (current-column))

(defun eiffel-previous-line-indent2 ()
  "Indentation of previous word, but negative"
  (backward-word 1)
  (back-to-indentation)
  (- 0 (current-column)))

(defun eiffel-indent-of-last-non-continuation-line ()
  "Amount of identation of last line that isn't a continuation"
  (save-excursion
    (while (or
            (eiffel-current-line-is-continuation)
            (eiffel-previous-line-is-continuation))
      (beginning-of-line)
      (backward-sexp))
    (back-to-indentation)
    (current-column)))

(defun eiffel-indent-assertion-continuation (id-colon)
  "Generally, are we in line that is a continuation of an assertion?
More precisely, are we inside a pre or a post condition clause on a
line that is a continuation of a multi-line assertion beginning with a
tag?  If so, return the indentation of the continuation line.  The
argument ID-COLON is t if the line we are indenting begins with
\"<id> :\", and nil otherwise."
  (let ((limit (point)))
  (if (save-excursion
    (if (re-search-backward
       (concat eiffel-feature-level-keywords-regexp "\\|"
         eiffel-end-keyword-regexp) nil t)
      (if (looking-at "ensure\\|require")
      (setq limit (point)))))
  (save-excursion
    (end-of-line)
    (if (and (not id-colon) (re-search-backward ": *" limit t))
      (progn
    (goto-char (match-end 0))
    (current-column)))))))

(defun eiffel-indent-assertion-tag ()
  "Return indentation for part of a multi-line assertion.
That is, the current line is assumed to be a continuation of a
multi-line assertion, and we return the required indentation."
  (save-excursion
  (if (re-search-backward "ensure\\|require\\|variant\\|invariant\\|check" nil t)
  (+ (eiffel-current-line-indent) eiffel-indent-increment)
    ;; This option should not occur
    (error "Could not find assertion tag"))))

(defun eiffel-matching-kw (matching-keyword-regexp)
  "Search backwards and return a keyword in MATCHING-KEYWORD-REGEXP.
Also set the value of variable `eiffel-matching-indent' to the
indentation of the keyword found.  If an `end' keyword occurs prior to
finding one of the keywords in MATCHING-KEYWORD-REGEXP and it
terminates a check clause, set the value of variable
`eiffel-matching-indent' to the indentation of the `end' minus the value
of `eiffel-check-keyword-indent'."
  (let* ((c "[^a-z0-9A-Z_.]")
   (search-regexp (concat c eiffel-end-keyword c "\\|"
        c matching-keyword-regexp))
   (keyword ""))
  (save-excursion
    ;; Search backward for a matching keyword.
    ;; Note that eiffel-non-indenting-keywords-regexp indicates we haven't
    ;; found a match so should keep going.
    (while (and (eiffel-re-search-backward search-regexp 1 t)
      (looking-at (concat c eiffel-non-indenting-keywords-regexp))
      (not (= (point) 1))))
    (if (looking-at search-regexp)
    ;; Then - a keyword was found
    (progn
    (setq keyword
      (buffer-substring (match-beginning 0) (match-end 0)))
    (if (and (looking-at eiffel-end-keyword-regexp)
       (eiffel-matching-line)
       (string-match eiffel-check-keyword eiffel-matching-kw-for-end))
    ;; Then
    (setq eiffel-matching-indent (- (eiffel-current-line-indent)
             (eiffel-check-keyword-indent-m)))
      ;; Else
      (setq eiffel-matching-indent (eiffel-current-line-indent))))
  ;; Else no keyword was found.  I think this is an error
  (setq eiffel-matching-indent 0)
  (message "No matching indent keyword was found"))
    keyword)))

(defun eiffel-line-contains-close-paren ()
  "Return t if the current line contains a close paren, nil otherwise.
If a close paren is found, the point is placed immediately after the
last close paren on the line.  If no paren is found, the point is
placed at the beginning of the line."
  (let ((search-min 0))
  (beginning-of-line)
  (setq search-min (point))
  (end-of-line)
  (if (search-backward ")" search-min t)
  ;; Then
  (progn
    (forward-char 1)
    t)
    ;; Else
    (beginning-of-line)
    nil)))

;; Not Currently Used
;;(defun eiffel-quoted-string-on-line-p ()
;;  "t if an Eiffel quoted string begins, ends, or is continued
;;   on current line."
;;  (save-excursion
;;    (beginning-of-line)
;;    ;; Line must either start with optional whitespace immediately followed
;;    ;; by a '%' or include a '\"'.  It must either end with a '%' character
;;    ;; or must include a second '\"' character.
;;    (looking-at "^\\([ \t]*%\\|[^\"\n]*\"\\)[^\"\n]*\\(%$\\|\"\\)")))

(defconst eiffel-opening-regexp
  "\\<\\(external\\|check\\|deferred\\|do\\|once\\|from\\|if\\|inspect\\|debug\\)\\>"
  "Keywords that open eiffel nesting constructs.")
;; OK, this is a horrible hack in all of this to handle "once" as a
;; special case because it has been overloaded.  The search for the
;; opening keyword on the current line is quite reasonably limited to
;; the current line.  Therefore, the standard hacky way that we avoid
;; matching once strings, by making sure they're followed by
;; whitespace and a non-double-quote, doesn't work here.
(defconst eiffel-non-opening-regexp
  "\\<once\\s-+\""
  "Pattern matching exclusions from `eiffel-opening-regexp'.")
(defconst eiffel-closing-regexp "\\<end\\>"
  "Keywords that close eiffel nesting constructs.")
(defconst eiffel-do-regexp "\\<\\(do\\|once\\|external\\)\\>"
  "Keyword that opens eiffel routine body.")
(defconst eiffel-opening-or-closing-regexp
  (concat "\\(" eiffel-opening-regexp "\\|" eiffel-closing-regexp "\\)")
  "Keywords that open or close eiffel nesting constructs.")

;;
;; Code to allow indenting whole eiffel blocks
;;

(defun eiffel-matching-line (&optional return-line-break direction)
  "Return the position of the keyword matching the one on the current line.
For example, a line containing the keyword `do' is matched by a line
containing the keyword `end' and a line containing `end' may be
matched by a number of opening keywords.  If the optional parameter
RETURN-LINE-BREAK is non-nil, the character position returned is the
beginning (or end) of the line containing the matching keyword instead
of the position of the keyword itself.  If the second optional
parameter, DIRECTION, is non-nil, the current line is not searched for
a keyword.  Instead, if the value of direction is 'forward, the
function acts as if an `eiffel-opening-regexp' is on the current line.
If the value of direction is 'backward, the function acts as if a
`eiffel-closing-regexp' is on the current line.  The effect of using the
direction parameter is to locate either the opening or closing keyword
of the syntactic construct containing the point."
  (let ((nesting-level 0)
  (search-end 0)
  matching-point opening-keyword match-start match-end
  success start-point)
  (unwind-protect
  (save-excursion
    (modify-syntax-entry ?_  "w  ")
    (setq eiffel-matching-kw-for-end "");; public variable set by this function
    (setq start-point (point))
    (end-of-line)
    (setq search-end (point))
    (beginning-of-line)
    ;; Set starting state: If direction was specified use it.
    ;; If direction is nil, search for a keyword on the current line
    ;; If the keyword is in eiffel-opening-regexp, set the search
    ;; direction to 'forward, if the keyword on the current line is `end'
    ;; set the search direction to 'backward.
    (cond ((eq direction 'forward)
     (end-of-line)       ;; So we wont see keywords on this line.
     (setq nesting-level 1))
    ((eq direction 'backward)
     (beginning-of-line) ;; So we wont see keywords on this line.
     (setq nesting-level -1))
    ((and (re-search-forward eiffel-opening-regexp search-end t)
        (eiffel-not-in-comment-or-quoted-string-p))
     (setq match-start (match-beginning 0))
     (setq match-end (match-end 0))
     (goto-char match-start)
     (if (and (not (looking-at eiffel-non-opening-regexp))
        (eiffel-not-in-comment-or-quoted-string-p))
       (setq nesting-level 1))
     (setq opening-keyword
         (cons (buffer-substring match-start match-end)
         opening-keyword))
     (goto-char match-end))
    ((and (progn (beginning-of-line) t)
        (re-search-forward eiffel-closing-regexp search-end t)
        (eiffel-not-in-comment-or-quoted-string-p))
     (goto-char (match-beginning 0))
     (if (eiffel-not-in-comment-or-quoted-string-p)
       (setq nesting-level -1))))
    ;; Perform the search
    (while (not (= nesting-level 0))
    (if (> nesting-level 0)
    ;; Then search forward for the next keyword not in a comment
    (while (and (re-search-forward eiffel-opening-or-closing-regexp nil 1)
        (goto-char (setq match-start (match-beginning 0)))
        (setq match-end   (match-end 0))
        (setq success t)
        (or (looking-at eiffel-non-opening-regexp)
        (eiffel-in-comment-or-quoted-string-p)))
      (goto-char match-end)
      (setq success nil))
      ;; Else search backward for the next keyword not in a comment
      (while (and (re-search-backward eiffel-opening-or-closing-regexp nil 1)
        (goto-char (setq match-start (match-beginning 0)))
        (setq success t)
        (or (looking-at eiffel-non-opening-regexp)
          (eiffel-in-comment-or-quoted-string-p)))
    (setq success nil)))
    (cond ((and (not (looking-at eiffel-non-opening-regexp))
      (looking-at eiffel-opening-regexp)
      success)
       ;; Found an opening keyword
       (if (> nesting-level 0)
         ;; Then
         (if (looking-at eiffel-do-regexp)
         ;; Then
         (setq nesting-level -1)
       ;; Else
       (setq opening-keyword
           (cons (buffer-substring match-start
                 (match-end 0))
           opening-keyword))
       (goto-char (match-end 0)))
       ;; Else
       (if (= nesting-level -1)
       ;; Then
       (progn
         (setq eiffel-matching-kw-for-end
         (buffer-substring match-start (match-end 0)))
         (if (looking-at "[ \t\n]+")
           (goto-char (match-end 0))))
         ;; Else
         (if (looking-at eiffel-do-regexp)
         ;; Then
         (progn
         (goto-char (eiffel-matching-line nil 'forward))
         (setq nesting-level -1))))
       (setq opening-keyword (cdr opening-keyword))
       (if return-line-break
       (beginning-of-line)))
       (setq nesting-level (1+ nesting-level)))
      ((and (looking-at eiffel-closing-regexp) success)
       ;; Found an opening keyword
       (if (> nesting-level 0)
         ;; Then
         (progn
       (setq opening-keyword (cdr opening-keyword))
       (if return-line-break
         (end-of-line))
       (goto-char (match-end 0)))
       ;; Else
       (setq opening-keyword
         (cons (buffer-substring (match-beginning 0)
               (match-end 0))
         opening-keyword)))
       (setq nesting-level (1- nesting-level)))
      (t (message (concat "Could not find match"
            (if (car opening-keyword)
            (concat " for: "
              (car opening-keyword)))))
       (goto-char start-point)
       (setq nesting-level 0))))
    (setq matching-point (point)))
    (modify-syntax-entry ?_  "_  "))
  (set-mark matching-point)))

;; ENHANCEME: Make this function correctly indent more than just routine
;;            bodies and their sub-constructs.  At the least it should
;;            handle whole routines also.
(defun eiffel-indent-construct ()
  "Indent an entire eiffel syntactic construct.
It is assumed that the point is within a nesting construct ('do',
`once', 'check', 'if', 'from', or 'inspect').  The whole construct is
indented up to the matching end.  If the point is not within such a
construct, then only that line is indented"
  (interactive)
  (let ((start-point 0) (end-point 0))
  (save-excursion
    (end-of-line)
    (if (not (= (point) (point-max))) (forward-char 1))
    (setq start-point (copy-marker (eiffel-matching-line t 'backward)))
    (goto-char start-point)
    (setq end-point   (eiffel-matching-line t 'forward))
    (eiffel-indent-region start-point end-point))))

(defun eiffel-indent-region (&optional start end)
  "Indent the lines in the current region.
The region may be specified using optional arguments START and END."
  (interactive)
  (let ((start-point (or start (region-beginning)))
  (end-point   (copy-marker (or end (region-end)))))
  (save-excursion
    (goto-char start-point)
    (cond ((eq major-mode 'eiffel-mode)
     (while (< (point) end-point)
       (if (not (looking-at "[ \t]*$"))
       (eiffel-indent-line))
       (forward-line 1)
       (if (< (point) end-point)
       (beginning-of-line))))
    (t (error "Buffer must be in eiffel mode"))))))

;;(defun eiffel-goto-matching-line (&optional direction)
;;  "Place the cursor on the line which closes(opens) the current
;;opening(closing) syntactic construct.  For example if the point
;;is on `from', executing goto-matching-line places the point
;;on the matching `end' and vice-versa."
;;  (interactive)
;;  (if (not direction)
;;      (progn
;;  (cond ((save-excursion (beginning-of-line) (looking-at "[ \t]*end.*$"))
;;         (goto-char (eiffel-matching-line nil 'backward)))
;;        ((looking-at "(")
;;         (forward-sexp))
;;        ((save-excursion (backward-char 1) (looking-at ")"))
;;         (backward-sexp))
;;        (t
;;         (goto-char (eiffel-matching-line nil 'forward)))))))

(defun eiffel-forward-sexp ()
  "Put cursor on line that closes the current opening syntactic construct.
For example, if the point is on `from' then the point is placed on the
matching `end'.  This also does matching of parens ala
\\[forward-sexp]."
  (interactive)
  (cond ((looking-at "[[(]")
   (forward-sexp))
  (t
   (goto-char (eiffel-matching-line nil 'forward)))))

(defun eiffel-backward-sexp ()
  "Put cursor on line that opens the current closing syntactic construct.
For example, if the point is on the terminating `end' of an `if'
statement, then the point is place on the opening `if'.  This also
does matching of parens ala \\[backward-sexp]'."
  (interactive)
  (cond ((eiffel-peeking-backwards-at "[])]")
   (backward-sexp))
  (t
   (goto-char (eiffel-matching-line nil 'backward)))))

(defun eiffel-local-indent (amount)
  "Set the value of `eiffel-indent-increment' to AMOUNT buffer-locally."
  (interactive "NNumber of spaces for eiffel-indent-increment: ")
  (make-local-variable 'eiffel-indent-increment)
  (setq eiffel-indent-increment amount))

;;------------------------------------------------------------------------
;; RFO Extras
;; See key map for bindings

;-------------------------
; Eiffel Class Template
;; Insert an Eiffel class template (per RFO coding standards)
(fset 'insert-eiffel-template
   [?\C-x ?i ?d ?: ?/ ?a ?c ?t ?i ?v ?e ?/ ?a ?m ?a ?l ?a ?s ?o ?f ?t ?/ ?t ?e ?m ?p ?l ?a ?t ?e ?s ?+ ?f ?o ?r ?m ?s ?/ ?e ?5 ?. ?t ?p ?l return ?\C-s ?c ?l ?a ?s ?s ?  ?\C-f ?\C-b])
;; (local-set-key "t" 'insert-eiffel-template)

;-------------------------
; Eiffel bar Comment

;; Insert a single feature separator (bar)
;; Position the cursor at the start of the first line of the
;; next feature
(fset 'eiffel-insert-bar-comment
      "\C-a\C-p\C-j\C-q\C-i--|\C-u62-\C-n\C-a\C-j")

;; Wrap an existing "feature" keyword line
;; Position the cursor at the start of the line to be wrapped
(fset 'eiffel-wrap-feature-kw
      "\C-a\C-j\C-p --|\C-u72=\C-n\C-j --|\C-u72=\C-n")

;; Wrap an existing "feature" keyword line (w/ a light wrapper)
;; Position the cursor at the start of the line to be wrapped
;(defalias 'eiffel-wrap-feature-kw-lt (read-kbd-macro
;"C-a RET C-p SPC --| C-u 72- C-n RET SPC --| C-u 72- C-n"))
(fset 'eiffel-wrap-feature-kw-lt
      "\C-a\C-j\C-p --|\C-u72-\C-n\C-j --|\C-u72-\C-n")

;-------------------------
; Eiffel Feature Separator

;; Insert a wrapped feature keyword line, with export status {NONE}
(fset 'eiffel-insert-none-feature-separator
      "\C-a\C-j\C-j\C-p\C-p--|\C-u72=\C-n\C-afeature {NONE} -- Add Comment\C-j--|\C-u72=\C-n\C-a")

;; Insert a wrapped feature keyword line, with export status ANY
(fset 'eiffel-insert-feature-separator
      "\C-a\C-j\C-j\C-p\C-p--|\C-u72=\C-n\C-afeature -- Add Comment\C-j--|\C-u72=\C-n\C-a")

;; Insert a dummy routine
(defalias 'eiffel-insert-dummy-routine (read-kbd-macro
      "C-a RET dummy RET -- RET require RET do RET ensure RET end RET C-p C-p C-p C-p C-p C-p C-@ C-n C-n C-n C-n C-n C-n ESC x indent-region RET C-p C-p C-p C-p C-p C-p C-a C-c b"))

;; Insert a dummy once function
(defalias 'eiffel-insert-dummy-once (read-kbd-macro
      "C-a RET dummy_once: SPC DUMMY RET -- RET once RET create SPC Result RET ensure RET exists: SPC Result SPC /= SPC Void RET end RET C-p C-p C-p C-p C-p C-p C-p C-@ C-n C-n C-n C-n C-n C-n C-n ESC x indent-region RET C-p C-p C-p C-p C-p C-p C-p C-a C-c b"))

;; Comment-out a line with an obvious, personal tag at its start
;; Add to your default.el a line like the following (uncommented)
;;(setq user-initials "RFO")
(fset 'eiffel-comment-line
       (if (boundp 'user-initials)
	   (concat "\C-a--" user-initials " \C-n\C-a")
	 "\C-a--| \C-n\C-a"
	 )
      )


;; ----------------------------------------------------------------------
;; This next portion of the file is derived from "eiffel.el"
;; Copyright (C) 1989, 1990 Free Software Foundation, Inc. and Bob Weiner
;; Available for use and distribution under the same terms as GNU Emacs.
;; ----------------------------------------------------------------------

(defvar eiffel-mode-map nil
  "Keymap for Eiffel mode.")

(if eiffel-mode-map
  nil
  (let ((map (make-sparse-keymap)))
;;  (define-key map [(control j)]       'newline-and-indent)
;;  (define-key map [(return)]          'reindent-then-newline-and-indent)
  (define-key map [(control j)]       'newline)
  (define-key map [(return)]          'newline)
  (define-key map [(meta control q)]  'eiffel-indent-construct)
  (define-key map [(meta \')]         'eiffel-feature-quote)
  (define-key map [(meta q)]          'eiffel-fill-paragraph)
  (define-key map [(meta control a)]  'eiffel-beginning-of-feature)
  (define-key map [(meta control e)]  'eiffel-end-of-feature)
  (define-key map [(control x) ?n ?d] 'eiffel-narrow-to-feature)
;; RFO extras
  (define-key map [(control c) ?b] 'eiffel-insert-bar-comment)
  (define-key map [(control c) ?f] 'eiffel-insert-feature-separator)
  (define-key map [(control c) ?F] 'eiffel-insert-none-feature-separator)
  (define-key map [(control c) ?o] 'eiffel-insert-dummy-once)
  (define-key map [(control c) ?r] 'eiffel-insert-dummy-routine)
  (define-key map [(control c) ?w] 'eiffel-wrap-feature-kw)
  (define-key map [(control c) ?W] 'eiffel-wrap-feature-kw-lt)
  (define-key map [(control c) ?x] 'eiffel-comment-line)
  (setq eiffel-mode-map map)))

(defvar eiffel-mode-syntax-table nil
  "Syntax table in use in Eiffel-mode buffers.")

(if eiffel-mode-syntax-table
  nil
  (let ((table (make-syntax-table))
  (i 0))
  (while (< i ?0)
    (modify-syntax-entry i "_   " table)
    (setq i (1+ i)))
  (setq i (1+ ?9))
  (while (< i ?A)
    (modify-syntax-entry i "_   " table)
    (setq i (1+ i)))
  (setq i (1+ ?Z))
  (while (< i ?a)
    (modify-syntax-entry i "_   " table)
    (setq i (1+ i)))
  (setq i (1+ ?z))
  (while (< i 128)
    (modify-syntax-entry i "_   " table)
    (setq i (1+ i)))
  (modify-syntax-entry ?  "    " table)
  (modify-syntax-entry ?-  ". 12" table)
  (modify-syntax-entry ?_  "_  " table)
  (modify-syntax-entry ?\t "    " table)
  (modify-syntax-entry ?\n ">   " table)
  (modify-syntax-entry ?\f ">   " table)
  (modify-syntax-entry ?\" "\"    " table)
  (modify-syntax-entry ?\\ "." table)
  (modify-syntax-entry ?\( "()  " table)
  (modify-syntax-entry ?\) ")(  " table)
  (modify-syntax-entry ?\[ "(]  " table)
  (modify-syntax-entry ?\] ")[  " table)
  (modify-syntax-entry ?\{ "(}  " table)
  (modify-syntax-entry ?\} "){  " table)
  (modify-syntax-entry ?' "\"" table)
  (modify-syntax-entry ?` "." table)
  (modify-syntax-entry ?/ "." table)
  (modify-syntax-entry ?* "." table)
  (modify-syntax-entry ?+ "." table)
  (modify-syntax-entry ?= "." table)
  (modify-syntax-entry ?% "\\" table)
  (modify-syntax-entry ?< "." table)
  (modify-syntax-entry ?> "." table)
  (modify-syntax-entry ?& "." table)
  (modify-syntax-entry ?| "." table)
  (modify-syntax-entry ?\; "." table)
  (modify-syntax-entry ?: "." table)
  (modify-syntax-entry ?! "." table)
  (modify-syntax-entry ?. "." table)
  (modify-syntax-entry ?, "." table)
  (setq eiffel-mode-syntax-table table)))

(defun eiffel-add-menu ()
  "Add the \"Eiffel\" menu to the menu bar."

  (easy-menu-define
   eiffel-mode-menu
   eiffel-mode-map
   "Menu for eiffel-mode."
   (append (list "Eiffel")
     (if eiffel-use-gnu-eiffel
       (list
    ["Compile..."            eiffel-compile t]
    ["Compiler Options..."   eiffel-set-compile-options t]
    ["Next Compile Error..." next-error  t]
    ["Run..."                eiffel-run     t]
    ["Debug..."              eiffel-debug   t]
    ["Short..."              eiffel-short   t]
    ["----------" nil nil]))
     (list
    ["Indent Construct"    eiffel-indent-construct t]
    ["----------" nil nil]
    (list "Imenu"
      ["By position"   eiffel-imenu-add-menubar-by-position t]
      ["By name"       eiffel-imenu-add-menubar-by-name     t])
    (list "Comments"
      ["Feature Quote" eiffel-feature-quote  (eiffel-in-comment-p)]
      ["Fill         " eiffel-fill-paragraph (eiffel-near-comment-p)])
    ["----------" nil nil]
      ["Customize"           eiffel-customize     t])))
  (easy-menu-add eiffel-mode-menu))

;;;###autoload
(defun eiffel-mode ()
  "Major mode for editing Eiffel programs.
\\[indent-for-tab-command] indents the current Eiffel line correctly and
\\[reindent-then-newline-and-indent] causes the current and next line to be
properly indented.

Key definitions:
\\{eiffel-mode-map}

If variable `eiffel-use-gnu-eiffel' is non-nil (default t) then support
for using GNU SmartEiffel is enabled.  Run \\[eiffel-customize] to see
compilation and indentation variables that can be customized."

  (interactive)

  (kill-all-local-variables)

  (setq major-mode 'eiffel-mode)
  (setq mode-name "Eiffel")

  (if eiffel-use-gnu-eiffel
    (progn
;;  (define-key eiffel-mode-map "\C-c\C-c" 'eiffel-compile)
;;  (define-key eiffel-mode-map "\C-c\C-o" 'eiffel-set-compile-options)
;;  (define-key eiffel-mode-map "\C-c\C-r" 'eiffel-run)
;;  (define-key eiffel-mode-map "\C-c\C-d" 'eiffel-debug)
;;  (define-key eiffel-mode-map "\C-c\C-s" 'eiffel-short)
)
;;  (define-key eiffel-mode-map "\C-c\C-c" nil)
;;  (define-key eiffel-mode-map "\C-c\C-o" nil)
;;  (define-key eiffel-mode-map "\C-c\C-r" nil)
;;  (define-key eiffel-mode-map "\C-c\C-s" nil)
)

  (use-local-map eiffel-mode-map)
  (eiffel-add-menu)
  (set-syntax-table eiffel-mode-syntax-table)

  ;; Make local variables.
  (make-local-variable 'paragraph-start)
  (make-local-variable 'paragraph-separate)
  (make-local-variable 'paragraph-ignore-fill-prefix)
  (make-local-variable 'require-final-newline)
  (make-local-variable 'parse-sexp-ignore-comments)
  (make-local-variable 'indent-line-function)
  (make-local-variable 'indent-region-function)
  (make-local-variable 'comment-start)
  (make-local-variable 'comment-end)
  (make-local-variable 'comment-column)
  (make-local-variable 'comment-start-skip)
  (make-local-variable 'font-lock-defaults)
  (make-local-variable 'imenu-create-index-function)
  ;; Now set their values.
  (setq paragraph-start              (concat "^$\\|" page-delimiter)
  paragraph-separate           paragraph-start
  paragraph-ignore-fill-prefix t
  require-final-newline        'ask
  parse-sexp-ignore-comments   t
  indent-line-function         'eiffel-indent-line
  indent-region-function       'eiffel-indent-region
  ;;
  comment-start                "-- "
  comment-end                  ""
  comment-column               32
  comment-start-skip           eiffel-comment-start-skip
  font-lock-defaults           eiffel-font-lock-defaults)

  (if eiffel-set-tab-width-flag
    (setq tab-width eiffel-indent-increment))

  (setq auto-fill-function 'eiffel-auto-fill)
  (run-hooks 'eiffel-mode-hook))

(defconst eiffel-prefeature-regexp
  (concat "\\(" eiffel-non-source-line "\\|\n\\)*" "[ \t]*")
  "Regexp matching whitespace-equivalent content, possibly before a feature.")

(defun eiffel-find-end-of-feature ()
  "Find the `end' of the current feature definition.
Assumes point is at the beginning of the feature, not in a comment or
quoted string."
  (let (ret)
  (modify-syntax-entry ?_  "w  ")
  (cond ((looking-at (concat eiffel-prefeature-regexp
           eiffel-routine-begin-regexp))
     ;; At the start of a routine, find matching end.
     (and (eiffel-re-search-forward eiffel-do-regexp nil t)
    (goto-char (match-beginning 0))
    (goto-char (setq ret (eiffel-matching-line)))))
    ((looking-at (concat eiffel-prefeature-regexp
           eiffel-probably-feature-regexp))
     ;; Not a routine, find end of attribute or constant.
     (goto-char (setq ret (match-end 0)))))
  (modify-syntax-entry ?_  "_  ")
  ret))

;; OK, this works well, but it doesn't work for the following cases:
;; * In the middle of the feature regexp that need to be matched.
;;   However, it doesn't need to since eiffel-beginning-of-feature adds
;;   some smarts around it...
(defun eiffel-find-beginning-of-feature ()
  "Find the beginning of the most recent feature definition.
This will always move backward, if possible."
  (interactive)

  (let ((start (point))
  candidate routine-begin)
  (if (eiffel-re-search-backward (concat "\\s-" eiffel-probably-feature-regexp)
        nil t)
  (progn
    (forward-char) ;; Skip the whitespace character matched above.
    (if (not (or (looking-at (concat
          "\\(" eiffel-attribute-regexp
          "\\|" eiffel-constant-regexp "\\)"))))
      ;; This is a routine.  Done.
      (point)
    ;; Variable/attribute or constant declaration matched.
    ;; Now we go back and find the previous routine start, the
    ;; following end, and see if the current position
    ;; (candidate) is between.  If it is, then candidate is a
    ;; variable or constant declaration within a routine, so
    ;; we're interested in the routine start.  If it isn't,
    ;; then it must be a class attribute or constant, so it is
    ;; what we're looking for.
    (setq candidate (point))
    (goto-char start)
    (if (eiffel-re-search-backward
     (concat "\\s-" eiffel-routine-begin-regexp) nil t)
    (progn
      (forward-char)
      (setq routine-begin (point))
      (eiffel-find-end-of-feature)
      (if (and (< routine-begin candidate)
         (< candidate (point)))
        (goto-char routine-begin)
      (goto-char candidate)))
      (goto-char candidate)))))))

(defun eiffel-beginning-of-feature (&optional arg)
  "Move backward to next feature beginning.
With ARG, do it that many times.  Negative arg -N
means move forward to Nth following beginning of feature.
Returns t unless search stops due to beginning or end of buffer."
  (interactive "p")

  (or arg
    (setq arg 1))

  (let ((start (point))
  (success t))
  (cond ((> arg 0)
     ;; Going backward.

     ;; We have to move forward to make sure we find any feature
     ;; that we might be in the middle of the beginning of.  How
     ;; far?  How about this far?
     (eiffel-re-search-forward eiffel-probably-feature-regexp nil 'move)

     ;; Change arg towards zero as we search, failing if we hit
     ;; edge of buffer.
     (while (and (> arg 0)
         (or (eiffel-find-beginning-of-feature)
         (setq success nil)))
     ;; If we've gone backwards from the original start, then
     ;; this counts.
     (if (< (point) start)
     (setq arg (1- arg))))
     (or success
       (goto-char (point-min))))

    ((< arg 0)
     ;; Going forward.

     ;; Similar to above, let's go back to the beginning of the
     ;; current feature, and then skip over features and find
     ;; the beginning of the next repeatedly.
     (eiffel-find-beginning-of-feature)

     (while (and (< arg 0)
         (or (not (eobp)) (setq success nil)))
     (eiffel-find-end-of-feature)
     (if (eiffel-re-search-forward eiffel-probably-feature-regexp
          nil 'move)
     (progn
       (goto-char (match-beginning 0))
       (if (> (point) start)
         (setq arg (1+ arg))))))))
  success))

(defun eiffel-end-of-feature (&optional arg)
  "Move forward to end of feature.
With argument, do it that many times.  Negative argument means move
back ARG preceding ends of features."
  (interactive "p")

  ;; Default is to find the first feature's end.
  ;; Huh?  Even if they specify 0?  - martin@meltin.net
  ;; Hmmm, it is what end-of-defun does...
  (if (or (null arg)
    (= arg 0))
    (setq arg 1))

  ;; This is a bad way of trying to get into position.  Happily, it
  ;; seems to work.  Hmmm, not sure if the comment skip is needed.
  (if (eiffel-in-comment-p)
    (end-of-line))
  (cond ((let ((curr (point)))
     (save-excursion
     (and (eiffel-beginning-of-feature)
      (eiffel-find-end-of-feature)
      (forward-line)
      (or (< curr (point))
        (and (< arg 0)
         (= curr (point)))))))
   ;; Within a feature.  Go to its beginning.
   (eiffel-beginning-of-feature))
  ((eiffel-peeking-backwards-at (concat "\\s-"
             eiffel-probably-feature-regexp))
   ;; Sitting at beginning of feature.  Don't move!
   t)
  (t
   ;; Not within a feature or at beginning, go to beginning of
   ;; next feature.
   (eiffel-beginning-of-feature -1)))

  ;; This part is correct.
  (if (eiffel-beginning-of-feature (+ (if (< arg 0) 0 1) (- arg)))
    (progn
  (eiffel-find-end-of-feature)
  (forward-line))))

(defun eiffel-narrow-to-feature ()
  "Make text outside current feature invisible.
The feature visible is the one that contains point or follows point."
  (interactive)
  (save-excursion
  (widen)
  (eiffel-end-of-feature)
  (let ((end (point)))
    (eiffel-beginning-of-feature)
    (narrow-to-region (point) end))))

(defun eiffel-current-line-indent ()
  "Return the indentation of the line containing the point."
  (save-excursion
  (back-to-indentation)
  (current-column)))

(defun eiffel-in-quoted-string-p (&optional non-strict-p)
  "Return t if point is in a quoted string.
Optional argument NON-STRICT-P if true causes the function to return
true even if the point is located in leading white space on a
continuation line.  Normally leading white space is not considered part
of the string."
  (let ((initial-regexp "^[ \t]*%\\|[^%]\"\\|%[ \t]*$")
  (search-limit (point))
  (count 0))
  (save-excursion
    ;; Line must either start with optional whitespace immediately followed
    ;; by a '%' or include a '\"' before the search-limit.
    (beginning-of-line)
    (while (re-search-forward initial-regexp search-limit t)
  (setq count (1+ count))
  (if (= count 1) (setq search-limit (1+ search-limit))))
    ;; If the number of quotes (including continuation line markers)
    ;; is odd, then we are inside of a string.  Also if non-strict-p
    ;; and we are in the leading white space of a continuation line,
    ;; then we are in a quote.
    (or (= (% count 2) 1)
    (progn
    (beginning-of-line)
    (and non-strict-p
     (looking-at "^[ \t]*%")))))))

;; ----------------------------------------------------------------------
;; End of portion derived from "eiffel.el"
;; ----------------------------------------------------------------------

(defun eiffel-comment-prefix ()
  "Return the prefix starting a comment that begins a line.
Comments that are not the only thing on a line return nil as their prefix."
  (save-excursion
  (end-of-line)
  (let ((limit (point)) len
    (in-string (eiffel-in-quoted-string-p)))
    (beginning-of-line)
    (cond ((re-search-forward "^[ \t]*--|?[ \t]*" limit t)
     (buffer-substring (match-beginning 0) (match-end 0)))
    ;; Handle string-literal continuation lines
    (in-string
     (end-of-line)
     (re-search-backward "^[ \t]*%\\|[^%]\"" nil t)
     (re-search-forward "%\\|\"" nil t)
     (setq len (1- (current-column)))
     (concat (make-string len ? ) "%"))))))

(defun eiffel-auto-fill ()
  "Auto-fill an Eiffel comment."
  (let ((fill-prefix (eiffel-comment-prefix))
  (pm (point-marker)))
  (if (and (> (current-column) fill-column)
     fill-prefix)
  (if (string-match "^[ \t]*%" fill-prefix)
    (progn
      (backward-char 1)
      (re-search-backward "[^][a-zA-Z0-9]" nil t)
      (forward-char 1)
      (insert "%\n")
      (insert fill-prefix)
      (goto-char pm))
    ;; (do-auto-fill)
    (backward-char 1)
    (re-search-backward "\\s-" nil t)
    (forward-char 1)
    (insert "\n")
    (insert fill-prefix)
    (goto-char pm)))))

(defun eiffel-fill-paragraph ()
  "Textually fills Eiffel comments ala \\[fill-paragraph]."
  (interactive)
  (save-excursion
  (let ((current-point (point))
    (fill-prefix (eiffel-comment-prefix))
    last-point para-begin para-end)
    (if fill-prefix
    (progn
    (setq last-point (point))
    (forward-line -1)
    (end-of-line)
    (while (and (not (= (point) last-point))
      (eiffel-comment-prefix))
      (setq last-point (point))
      (forward-line -1)
      (end-of-line))
    (if (= (point) last-point)
    (setq para-begin (save-excursion (beginning-of-line) (point)))
      (setq para-begin (1+ (point))))
    (goto-char current-point)
    (setq last-point (point))
    (forward-line 1)
    (end-of-line)
    (while (and (not (= (point) last-point))
      (eiffel-comment-prefix))
      (setq last-point (point))
      (forward-line 1)
      (end-of-line))
    (if (= (point) last-point)
    (setq para-end (point))
      (beginning-of-line)
      (setq para-end (point)))
    ;; Avert eyes now - gross hack follows...  how big can an
    ;; Eiffel comment be anyway?  :-)
    (let ((orig-region (and (<= (- para-end para-begin)
          eiffel-fill-max-save)
          (buffer-substring para-begin para-end)))
      (orig-state  (buffer-modified-p))
      (ret  (fill-region para-begin para-end)))
      (and orig-region
       (<= para-end (point-max))
       (string-equal
      orig-region (buffer-substring para-begin para-end))
       (set-buffer-modified-p orig-state))
      ret))))))

(defun eiffel-indent-line (&optional whole-exp)
  "Indent the current line as Eiffel code.
With optional argument WHOLE-EXP, indent any additional lines of the
same clause rigidly along with this one (not implemented yet)."
  (interactive "p")
  (save-excursion
  (beginning-of-line)
  (skip-chars-forward " \t")
  (let ((indent (eiffel-calc-indent)))
    (if (not (= indent (current-column)))
    (progn
    (delete-horizontal-space)
    (indent-to indent)))))
  (skip-chars-forward " \t"))

(defun eiffel-move-to-prev-non-blank ()
  "Move point to previous line excluding blank lines.
Return t if successful, nil if not."
  (beginning-of-line)
  (re-search-backward "^[ \t]*[^ \t\n]" nil t))

(defun eiffel-in-multiline-string-expression ()
  "Determine if we are inside a multi-line string expression. Searches a maximu m of 2048 characters backward, so will not work for really large strings."
  (interactive)
  (let (multi-line-string (limit 0))
  (if (>= (point) 2048)
    (setq limit (- (point) 2048)))
  (save-excursion
    (re-search-backward "\\([^%]\"[[]\n\\|\n[ \t]*[]]\"\\)" limit t)
    (if (looking-at "[ \t]\"[[]\n")
      (setq multi-line-string t)))
  multi-line-string))

(defvar eiffel-last-feature-level-indent -1)
(defvar eiffel-feature-level-indent-regexp nil)
(defun eiffel-in-paren-expression ()
  "Determine if we are inside of a parenthesized expression. Will return invalid data if called while inside a string."
  (interactive)
  (let ((paren-count 0) (limit 0))
  (save-excursion
    (if (= eiffel-last-feature-level-indent (eiffel-feature-level-indent-m))
    (setq limit
    (re-search-backward eiffel-feature-level-indent-regexp nil t))
  (setq eiffel-last-feature-level-indent (eiffel-feature-level-indent-m))
  (setq eiffel-feature-level-indent-regexp
      (concat "^" (make-string eiffel-last-feature-level-indent ? )
        "[^ \t\n]"))
  (setq limit
      (or (re-search-backward eiffel-feature-level-indent-regexp nil t)
      0))))
  (save-excursion
    (while (re-search-backward "[][()]" limit t)
  (if (looking-at "[[(]")
    (setq paren-count (1+ paren-count))
    (setq paren-count (1- paren-count)))))
  paren-count))

(defun eiffel-manifest-array-common ()
  "Common code for handling indentation/presence of Eiffel manifest arrays."
  (let ((paren-count 0))
  (if (= eiffel-last-feature-level-indent (eiffel-feature-level-indent-m))
  nil
    (setq eiffel-last-feature-level-indent (eiffel-feature-level-indent-m))
    (setq eiffel-feature-level-indent-regexp
    (concat "^" (make-string eiffel-last-feature-level-indent ? )
      "[^ \t\n]")))
  (while (and (<= paren-count 0) (re-search-backward "<<\\|>>" nil t))
    (if (not (eiffel-peeking-backwards-at "|\\|@"))
    (if (looking-at "<<")
      (setq paren-count (1+ paren-count))
    (setq paren-count (1- paren-count)))))
  paren-count))

(defun eiffel-manifest-array-indent ()
  "Determine if we are inside of a manifest array."
  (interactive)
  (let (indent)
  (save-excursion
    (if (> (eiffel-manifest-array-common) 0)
    (let ((eol (save-excursion (end-of-line) (point))))
    (setq indent
      (or (and (re-search-forward "[^< \t]" eol t)
         (1- (current-column)))
        (+ (current-column) 2))))))
  indent))

(defun eiffel-manifest-array-start ()
  "Determine the indentation of the statement containing a manifest array."
  (interactive)
  (let (indent)
  (save-excursion
    (if (> (eiffel-manifest-array-common) 0)
    (let ((limit (progn (end-of-line) (point))))
    (beginning-of-line)
    (if (re-search-forward "^[ \t]*<<" limit t)
    (setq indent (- (current-column) 2 eiffel-indent-increment))
      (re-search-forward "^[ \t]*" limit t)
      (setq indent (current-column))))))
  indent))

;; ----------------------------------------------------------------------
;; The function below is derived from "eiffel-mult-fmt.el"
;; Copyright (C) 1985 Free Software Foundation, Inc.
;; Copyright (C) 1990 Bob Weiner, Motorola Inc.
;; Available for use and distribution under the same terms as GNU Emacs.
;; ----------------------------------------------------------------------

(defun eiffel-indent-multi-line (&optional parse-start)
  "Return indentation for line within parentheses or double quotes.
That is, we are in a multi-line parenthesised or double-quoted
expression, and want to know the suggested indentation for the current
line.  If we are not within such an expression then return -1.
Optional argument PARSE-START is buffer position at which to begin
parsing, default is to begin at the feature enclosing or preceding
point."
  (let ((eiffel-opoint (point))
  (indent-point (progn (beginning-of-line) (point)))
  (eiffel-ind-val -1)
  (eiffel-in-str nil)
  (eiffel-paren-depth 0)
  (retry t)
  state
  ;; setting this to a number inhibits calling hook
  last-sexp containing-sexp)
  (if parse-start
  (goto-char parse-start)
    (eiffel-beginning-of-feature))
  ;; Find outermost containing sexp
  (while (< (point) indent-point)
    (setq state (parse-partial-sexp (point) indent-point 0)))
  ;; Find innermost containing sexp
  (while (and retry
    state
    (> (setq eiffel-paren-depth (elt state 0)) 0))
    (setq retry nil)
    (setq last-sexp (elt state 2))
    (setq containing-sexp (elt state 1))
    ;; Position following last unclosed open.
    (goto-char (1+ containing-sexp))
    ;; Is there a complete sexp since then?
    (if (and last-sexp (> last-sexp (point)))
    ;; Yes, but is there a containing sexp after that?
    (let ((peek (parse-partial-sexp last-sexp indent-point 0)))
    (if (setq retry (car (cdr peek))) (setq state peek)))))
  (if retry
  nil
    ;; Innermost containing sexp found
    (goto-char (1+ containing-sexp))
    (if (not last-sexp)
    ;; indent-point immediately follows open paren.
    nil
  ;; Find the start of first element of containing sexp.
  (parse-partial-sexp (point) last-sexp 0 t)
  (cond ((looking-at "\\s(")
       ;; First element of containing sexp is a list.
       ;; Indent under that list.
       )
      ((> (save-excursion (forward-line 1) (point))
      last-sexp)
       ;; This is the first line to start within the containing sexp.
       (backward-prefix-chars))
      (t
       ;; Indent beneath first sexp on same line as last-sexp.
       ;; Again, it's almost certainly a routine call.
       (goto-char last-sexp)
       (beginning-of-line)
       (parse-partial-sexp (point) last-sexp 0 t)
       (backward-prefix-chars))))
    (setq eiffel-ind-val (current-column)))
  ;; Point is at the point to indent under unless we are inside a string.
  (setq eiffel-in-str (elt state 3))
  (goto-char eiffel-opoint)
  (if (not eiffel-in-str)
  nil
    ;; Inside a string, indent 1 past string start
    (setq eiffel-paren-depth 1);; To account for being inside string
    (save-excursion
  (if (re-search-backward "\"" nil t)
    (if eiffel-indent-string-continuations-relatively-flag
    (setq eiffel-ind-val (1+ (current-column)))
      (setq eiffel-ind-val (eiffel-current-line-indent)))
    (goto-char indent-point)
    (if (looking-at "^[ \t]*[^ \t\n]")
      (eiffel-move-to-prev-non-blank))
    (skip-chars-forward " \t")
    (setq eiffel-ind-val (current-column)))))
  (if (> eiffel-paren-depth 0) eiffel-ind-val -1)))

;; ----------------------------------------------------------------------
;; imenu support, great for browsing foreign code.
;; Originally contributed by Berend de Boer <berend@pobox.com>.
;; ----------------------------------------------------------------------

(defun eiffel-imenu-add-menubar-by-position ()
  "Add menu of features of a class, sorted in order of occurence."
  (interactive)
  (setq imenu-create-index-function  'eiffel-imenu-create-index-by-position)
  (imenu-add-to-menubar "Eiffel features")
  )

(defun eiffel-imenu-add-menubar-by-name ()
  "Add menu of features of a class, sorted by name."
  (interactive)
  (setq imenu-create-index-function  'eiffel-imenu-create-index-by-name)
  (imenu-add-to-menubar "Eiffel names"))

(defun eiffel-imenu-create-index-by-position ()
  "Generate index of features of a class, sorted in order of occurence."
  (eiffel-imenu-create-index 0))

(defun eiffel-imenu-create-index-by-name ()
  "Generate index of features of a class, sorted by name."
  (eiffel-imenu-create-index 1))

(defun eiffel-imenu-create-index (sort-method)
  "Generate an index of all features of a class.
Sort by position if sort-method is 0. Sort by name if sort-method is 1."

  (let (menu prevpos)

  (imenu-progress-message prevpos 0 t)

  ;; scan for features
  (goto-char (point-max))
  (while (eiffel-find-beginning-of-feature)
    (imenu-progress-message prevpos nil t)
    (if (looking-at "\\(\\sw\\|\\s_\\)+")
    (add-to-list 'menu (cons (buffer-substring-no-properties
          (match-beginning 0)
          (match-end 0)) (point)))))

  (imenu-progress-message prevpos 100)

  ;; sort in increasing buffer position order or by name
  (if (= sort-method 0)
    (sort menu (function (lambda (a b) (< (cdr a) (cdr b)))))
    (sort menu (function (lambda (a b) (string< (car a) (car b))))))))

;; XEmacs addition
;;;###autoload(add-to-list 'auto-mode-alist '("\\.e\\'" . eiffel-mode))

(provide 'eiffel-mode)
;;; eiffel-mode.el ends here
