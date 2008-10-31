;; Emacs configuration file
;; .emacs.el, main user startup file
;; Written by Martin Ebourne
;; $Id$

(if (file-readable-p "/etc/config/emacs/startup.el")
    (load "/etc/config/emacs/startup")
  (load "~/config/emacs/startup"))
(load custom-file)

;; Enable useful commands which are disabled by default
(put 'downcase-region 'disabled nil)
(put 'eval-expression 'disabled nil)
(put 'narrow-to-page 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'scroll-left 'disabled nil)
(put 'set-goal-column 'disabled nil)
(put 'upcase-region 'disabled nil)

;; Disable the useless toolbar
(tool-bar-mode nil)

;; Keep cursor on middle line
(setq constrain-percentage 50)

;; Can't set in X defaults since spacing goes all wrong
;(set-frame-font "7x13")
;(set-frame-height (selected-frame) 51)
;(set-frame-width (selected-frame) 101)

(setq gud-dbx-directories
      (list
       "/view/main.mebourne/vobs/bpta/BPTA_CAS_CORE/Source/adaptor"
       "/view/main.mebourne/vobs/bpta/BPTA_CAS_CORE/Source/apps"
       "/view/main.mebourne/vobs/bpta/BPTA_CAS_CORE/Source/common"
       "/view/main.mebourne/vobs/bpta/BPTA_CAS_CORE/Source/converter"
       "/view/main.mebourne/vobs/bpta/BPTA_CAS_CORE/Source/dom"
       "/view/main.mebourne/vobs/bpta/BPTA_CAS_CORE/Source/message"
       "/view/main.mebourne/vobs/bpta/BPTA_CAS_CORE/Source/oracle"
       "/view/main.mebourne/vobs/bpta/BPTA_CAS_CORE/Source/soap"
       "/view/main.mebourne/vobs/bpta/BPTA_CAS_CORE/Source/tests"
       "/home/mebourne/work/pathan/current/src/DOMutils"
       "/home/mebourne/work/pathan/current/src/config"
       "/home/mebourne/work/pathan/current/src/config/win32"
       "/home/mebourne/work/pathan/current/src/context"
       "/home/mebourne/work/pathan/current/src/dataItem"
       "/home/mebourne/work/pathan/current/src/defs"
       "/home/mebourne/work/pathan/current/src/dom3-xpath"
       "/home/mebourne/work/pathan/current/src/exceptions"
       "/home/mebourne/work/pathan/current/src/factory"
       "/home/mebourne/work/pathan/current/src/functionAPI"
       "/home/mebourne/work/pathan/current/src/interface"
       "/home/mebourne/work/pathan/current/src/lexer"
       "/home/mebourne/work/pathan/current/src/navigation"
       "/home/mebourne/work/pathan/current/src/parser"
       "/home/mebourne/work/pathan/current/src/simpleVariables"
       "/home/mebourne/work/pathan/current/src/testharness"
       "/view/main.mebourne/vobs/foboca/FOBOCA_API/Source/code"
       "/view/main.mebourne/vobs/foboca/FOBOCA_API/Source/code/dispatchers"
       "/view/main.mebourne/vobs/foboca/FOBOCA_API/Source/code/event"
       "/view/main.mebourne/vobs/foboca/FOBOCA_API/Source/code/fboctrace"
       "/view/main.mebourne/vobs/foboca/FOBOCA_API/Source/code/fobocom"
       "/view/main.mebourne/vobs/foboca/FOBOCA_API/Source/code/gaia"
       "/view/main.mebourne/vobs/foboca/FOBOCA_API/Source/code/interface"
       "/view/main.mebourne/vobs/foboca/FOBOCA_API/Source/code/logger"
       "/view/main.mebourne/vobs/foboca/FOBOCA_API/Source/code/middleware"
       "/view/main.mebourne/vobs/foboca/FOBOCA_API/Source/code/middleware/interfaces"
       "/view/main.mebourne/vobs/foboca/FOBOCA_API/Source/code/middleware/mqimp"
       "/view/main.mebourne/vobs/foboca/FOBOCA_API/Source/code/middleware/ssimp"
       "/view/main.mebourne/vobs/foboca/FOBOCA_API/Source/code/msg/converters"
       "/view/main.mebourne/vobs/foboca/FOBOCA_API/Source/code/msg/formats"
       "/view/main.mebourne/vobs/foboca/FOBOCA_API/Source/code/msg/formats/gsdd"
       "/view/main.mebourne/vobs/foboca/FOBOCA_API/Source/code/msg/formats/sdd"
       "/view/main.mebourne/vobs/foboca/FOBOCA_API/Source/code/msg/internalmsgs"
       "/view/main.mebourne/vobs/foboca/FOBOCA_API/Source/code/msgmap"
       "/view/main.mebourne/vobs/foboca/FOBOCA_API/Source/code/msgvalidation"
       "/view/main.mebourne/vobs/foboca/FOBOCA_API/Source/code/namingservice"
       "/view/main.mebourne/vobs/foboca/FOBOCA_API/Source/code/utils"
       "/view/main.mebourne/vobs/foboca/FOBOCA_API/Source/code/utils/factory"
       "/view/main.mebourne/vobs/foboca/FOBOCA_API/Source/code/vcs"
       "/view/main.mebourne/vobs/foboca/FOBOCA_API/Source/code/xmldom"
       "/home/mebourne/dloads/xml/xerces-c-src1_7_0/src/xercesc"
       "/home/mebourne/dloads/xml/xerces-c-src1_7_0/src/xercesc/com"
       "/home/mebourne/dloads/xml/xerces-c-src1_7_0/src/xercesc/dom"
       "/home/mebourne/dloads/xml/xerces-c-src1_7_0/src/xercesc/framework"
       "/home/mebourne/dloads/xml/xerces-c-src1_7_0/src/xercesc/idom"
       "/home/mebourne/dloads/xml/xerces-c-src1_7_0/src/xercesc/internal"
       "/home/mebourne/dloads/xml/xerces-c-src1_7_0/src/xercesc/parsers"
       "/home/mebourne/dloads/xml/xerces-c-src1_7_0/src/xercesc/sax"
       "/home/mebourne/dloads/xml/xerces-c-src1_7_0/src/xercesc/sax2"
       "/home/mebourne/dloads/xml/xerces-c-src1_7_0/src/xercesc/util"
       "/home/mebourne/dloads/xml/xerces-c-src1_7_0/src/xercesc/util/Compilers"
       "/home/mebourne/dloads/xml/xerces-c-src1_7_0/src/xercesc/util/MsgLoaders/ICU"
       "/home/mebourne/dloads/xml/xerces-c-src1_7_0/src/xercesc/util/MsgLoaders/InMemory"
       "/home/mebourne/dloads/xml/xerces-c-src1_7_0/src/xercesc/util/MsgLoaders/MsgCatalog"
       "/home/mebourne/dloads/xml/xerces-c-src1_7_0/src/xercesc/util/MsgLoaders/MsgFile"
       "/home/mebourne/dloads/xml/xerces-c-src1_7_0/src/xercesc/util/MsgLoaders/Win32"
       "/home/mebourne/dloads/xml/xerces-c-src1_7_0/src/xercesc/util/NetAccessors/MacOSURLAccess"
       "/home/mebourne/dloads/xml/xerces-c-src1_7_0/src/xercesc/util/NetAccessors/MacOSURLAccessCF"
       "/home/mebourne/dloads/xml/xerces-c-src1_7_0/src/xercesc/util/NetAccessors/Socket"
       "/home/mebourne/dloads/xml/xerces-c-src1_7_0/src/xercesc/util/NetAccessors/WinSock"
       "/home/mebourne/dloads/xml/xerces-c-src1_7_0/src/xercesc/util/NetAccessors/libWWW"
       "/home/mebourne/dloads/xml/xerces-c-src1_7_0/src/xercesc/util/Platforms/AIX"
       "/home/mebourne/dloads/xml/xerces-c-src1_7_0/src/xercesc/util/Platforms/FreeBSD"
       "/home/mebourne/dloads/xml/xerces-c-src1_7_0/src/xercesc/util/Platforms/HPUX"
       "/home/mebourne/dloads/xml/xerces-c-src1_7_0/src/xercesc/util/Platforms/IRIX"
       "/home/mebourne/dloads/xml/xerces-c-src1_7_0/src/xercesc/util/Platforms/Linux"
       "/home/mebourne/dloads/xml/xerces-c-src1_7_0/src/xercesc/util/Platforms/MacOS"
       "/home/mebourne/dloads/xml/xerces-c-src1_7_0/src/xercesc/util/Platforms/OS2"
       "/home/mebourne/dloads/xml/xerces-c-src1_7_0/src/xercesc/util/Platforms/OS390"
       "/home/mebourne/dloads/xml/xerces-c-src1_7_0/src/xercesc/util/Platforms/OS400"
       "/home/mebourne/dloads/xml/xerces-c-src1_7_0/src/xercesc/util/Platforms/OpenServer"
       "/home/mebourne/dloads/xml/xerces-c-src1_7_0/src/xercesc/util/Platforms/PTX"
       "/home/mebourne/dloads/xml/xerces-c-src1_7_0/src/xercesc/util/Platforms/Solaris"
       "/home/mebourne/dloads/xml/xerces-c-src1_7_0/src/xercesc/util/Platforms/Tandem"
       "/home/mebourne/dloads/xml/xerces-c-src1_7_0/src/xercesc/util/Platforms/Tru64"
       "/home/mebourne/dloads/xml/xerces-c-src1_7_0/src/xercesc/util/Platforms/UnixWare"
       "/home/mebourne/dloads/xml/xerces-c-src1_7_0/src/xercesc/util/Platforms/Win32"
       "/home/mebourne/dloads/xml/xerces-c-src1_7_0/src/xercesc/util/Transcoders/ICU"
       "/home/mebourne/dloads/xml/xerces-c-src1_7_0/src/xercesc/util/Transcoders/Iconv"
       "/home/mebourne/dloads/xml/xerces-c-src1_7_0/src/xercesc/util/Transcoders/Iconv390"
       "/home/mebourne/dloads/xml/xerces-c-src1_7_0/src/xercesc/util/Transcoders/Iconv400"
       "/home/mebourne/dloads/xml/xerces-c-src1_7_0/src/xercesc/util/Transcoders/IconvFBSD"
       "/home/mebourne/dloads/xml/xerces-c-src1_7_0/src/xercesc/util/Transcoders/MacOSUnicodeConverter"
       "/home/mebourne/dloads/xml/xerces-c-src1_7_0/src/xercesc/util/Transcoders/Win32"
       "/home/mebourne/dloads/xml/xerces-c-src1_7_0/src/xercesc/util/regx"
       "/home/mebourne/dloads/xml/xerces-c-src1_7_0/src/xercesc/validators/DTD"
       "/home/mebourne/dloads/xml/xerces-c-src1_7_0/src/xercesc/validators/common"
       "/home/mebourne/dloads/xml/xerces-c-src1_7_0/src/xercesc/validators/datatype"
       "/home/mebourne/dloads/xml/xerces-c-src1_7_0/src/xercesc/validators/schema"
       "/home/mebourne/dloads/xml/xerces-c-src1_7_0/src/xercesc/validators/schema/identity"
       ))
