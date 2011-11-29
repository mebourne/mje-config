;; Emacs configuration file
;; Syntax colouring regular expressions for sql-mode
;; Written by Martin Ebourne

(defvar sql-mode-sybase-font-lock-keywords nil
  "Sybase SQL keywords used by font-lock.")
(if sql-mode-sybase-font-lock-keywords
    ()
  (let ((ansi-keywords
	 (eval-when-compile (regexp-opt '(
"absolute" "action" "allocate" "are" "assertion" "bit_length"
"both" "cascaded" "case" "cast" "catalog" "char_length"
"character" "character_length" "coalesce" "collate" "collation"
"column" "connection" "constraints" "corresponding" "cross"
"current_date" "current_time" "current_timestamp"
"current_user" "date" "day" "dec" "deferrable" "deferred"
"describe" "descriptor" "diagnostics" "disconnect" "domain"
"end-exec" "exception" "extract" "false" "first" "found"
"full" "get" "global" "go" "hour" "immediate" "indicator" "initially"
"inner" "input" "insensitive" "integer" "interval" "isnull" "join"
"language" "last" "leading" "left" "local" "lower" "match" "minute"
"module" "month" "names" "natural" "nchar" "next" "no" "nullif"
"octet_length" "outer" "output" "overlaps" "pad" "partial"
"position" "preserve" "prior" "relative" "restrict" "right"
"scroll" "second" "section" "session_user" "size" "space"
"sql" "sqlcode" "sqlerror" "sqlstate" "substring" "system_user"
"then" "time" "timezone_hour" "timezone_minute"
"trailing" "translate" "translation" "trim" "true" "unknown" "upper"
"usage" "value" "when" "whenever" "write" "year" "zone"
))))
	(ansi-type-names
	 (eval-when-compile (regexp-opt '(
"char" "decimal" "float" "int" "numeric" "real" "smallint" "varchar" 
))))
	(sybase-keywords
	 (eval-when-compile (regexp-opt '(
"add" "all" "alter" "and" "any" "arith_overflow" "as" "asc" "at"
"authorization" "avg" "begin" "between" "break" "browse" "bulk" "by"
"cascade" "case" "char_convert" "check" "checkpoint" "close"
"clustered" "coalesce" "commit" "compute" "confirm" "connect"
"constraint" "continue" "controlrow" "convert" "count" "create"
"current" "cursor" "database" "dbcc" "deallocate" "declare"
"default" "delete" "desc" "disk" "distinct" "double" "drop" "dummy"
"dump" "else" "end" "endtran" "errlvl" "errordata" "errorexit"
"escape" "except" "exclusive" "exec" "execute" "exists" "exit"
"exp_row_size" "external" "fetch" "fillfactor" "for" "foreign"
"from" "goto" "grant" "group" "having" "holdlock" "identity"
"identity_gap" "identity_insert" "identity_start" "if" "in"
"index" "insert" "install" "intersect" "into" "is" "isolation" "jar"
"join" "key" "kill" "level" "like" "lineno" "load" "lock" "max"
"max_rows_per_page" "min" "mirror" "mirrorexit" "modify"
"national" "noholdlock" "nonclustered" "not" "null" "nullif"
"numeric_truncation" "of" "off" "offsets" "on" "once" "online" "only"
"open" "option" "or" "order" "over" "partition" "perm" "permanent"
"plan" "precision" "prepare" "primary" "print" "privileges" "proc"
"procedure" "processexit" "proxy_table" "public" "quiesce"
"raiserror" "read" "readpast" "readtext" "reconfigure"
"references" "remove" "reorg" "replace" "replication"
"reservepagegap" "return" "revoke" "role" "rollback" "rowcount"
"rows" "rule" "save" "schema" "select" "set" "setuser" "shared"
"shutdown" "some" "statistics" "stripe" "sum" "syb_identity"
"syb_restree" "table" "temp" "temporary" "textsize" "to" "tran"
"transaction" "trigger" "truncate" "tsequal" "union" "unique"
"unpartition" "update" "use" "user" "user_option" "using" "values"
"varying" "view" "waitfor" "where" "while" "with" "work"
"writetext"
))))
	(sybase-type-names
	 (eval-when-compile (regexp-opt '(
"binary" "bit" "datetime" "image" "money" "nchar" "nvarchar"
"smalldatetime" "smallmoney" "sysname" "text" "timestamp"
"tinyint" "varbinary"
))))
	(token "[a-zA-Z_][a-zA-Z0-9_]*")
	)
    (setq sql-mode-sybase-font-lock-keywords
	  (list
	   ;;
	   ;; Fontify otherwise as symbol names, and the preprocessor directive names.
	   (list (concat "^[ \t]*\\(#[ \t]*[a-z]+\\)\\>[ \t]*\\(" token "\\)?")
		 '(1 font-lock-constant-face) '(2 font-lock-variable-name-face nil t))
	   ;;
	   ;; Fontify all builtin keywords
	   (cons (concat "\\<\\(" ansi-keywords "\\)\\>") 'font-lock-keyword-face)
	   (cons (concat "\\<\\(" sybase-keywords "\\)\\>") 'font-lock-keyword-face)
	   ;;
	   ;; Fontify all types
	   (cons (concat "\\<\\(" ansi-type-names "\\)\\>") 'font-lock-type-face)
	   (cons (concat "\\<\\(" sybase-type-names "\\)\\>") 'font-lock-type-face)
	   (cons (concat "\\<\\(dm_[a-zA-Z0-9_]*\\)\\>") 'font-lock-type-face)
	   ;;
	   ;; Fontify numbers
	   (list (concat "\\<\\([0-9][0-9A-Fa-fXxUuLlEe]*\\)\\>") 1 'font-lock-number-face)
	   ;;
	   ;; Fontify function names
	   ;(list (concat "\\<\\(" token "\\)[ \t]*(") 1 'font-lock-function-name-face)
	   ;;
	   ;; Fontify variable names
	   (cons (concat "\\(@+" token "\\)\\>") 'font-lock-function-name-face)
	   (list (concat "\\<\\(" token "\\)\\>") 1 'font-lock-variable-name-face)
	   ))))
