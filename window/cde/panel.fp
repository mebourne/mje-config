###############################################################################
#
#  dtwm.fp
#
#  This file contains a full definition for the default front panel.
#
#  (c) Copyright 1993, 1994, 1995 Hewlett-Packard Company
#  (c) Copyright 1993, 1994, 1995 International Business Machines Corp.
#  (c) Copyright 1993, 1994, 1995 Sun Microsystems, Inc.
#  (c) Copyright 1993, 1994, 1995 Novell, Inc.
#
###############################################################################
set DtDbVersion=1.0

PANEL FrontPanel
{
  DISPLAY_HANDLES	 True
  DISPLAY_MENU		 True
  DISPLAY_MINIMIZE	 True
  CONTROL_BEHAVIOR	 single_click
  DISPLAY_CONTROL_LABELS False
  HELP_TOPIC		 FPOnItemFrontPanel
  HELP_VOLUME		 FPanel
}


BOX Top
{
  CONTAINER_NAME	FrontPanel
  POSITION_HINTS	first
  HELP_TOPIC		FPOnItemBox
  HELP_VOLUME		FPanel
}


CONTROL Clock
{
  TYPE			clock
  CONTAINER_NAME	Top
  CONTAINER_TYPE	BOX
  POSITION_HINTS	1
  ICON                  Fpclock
  LABEL                 Clock
  HELP_TOPIC		FPOnItemClock
  HELP_VOLUME		FPanel
}

CONTROL Date
{
  TYPE			date
  CONTAINER_NAME	Top
  CONTAINER_TYPE	BOX
  POSITION_HINTS	2
  ICON			FpCM
  LABEL                 Calendar
  DATE_FORMAT		%b%n%e
  PUSH_ACTION		Dtcm
  PUSH_RECALL		True
  DROP_ACTION		DtcmInsert
  CLIENT_NAME		dtcm
  HELP_TOPIC		FPOnItemDate
  HELP_VOLUME		FPanel
}

CONTROL Home
{
  TYPE			icon
  CONTAINER_NAME	Top
  CONTAINER_TYPE	BOX
  POSITION_HINTS	3
  ICON			Fphome
  LABEL			Home Folder
  PUSH_ACTION		DtfileHome
  DROP_ACTION		Dtfile
  HELP_TOPIC		FPOnItemHome
  HELP_VOLUME		FPanel
  DELETE		True
}


CONTROL TextEditor
{
  TYPE			icon
  CONTAINER_NAME	Top
  CONTAINER_TYPE	BOX
  POSITION_HINTS	4
  ICON			Fppenpd
  LABEL			Text Editor
  PUSH_ACTION		TextEditor
  DROP_ACTION		TextEditor
  HELP_TOPIC		FPOnItemTextEditor
  HELP_VOLUME		FPanel
  DELETE		True
}

CONTROL Mail
{
  TYPE			icon
  CONTAINER_NAME	Top
  CONTAINER_TYPE	BOX
  POSITION_HINTS	5
  ICON			DtMail
  LABEL                 Mail
  ALTERNATE_ICON	DtMnew
  MONITOR_TYPE		mail
  DROP_ACTION       	Compose
  PUSH_ACTION		Dtmail
  PUSH_RECALL		true
  CLIENT_NAME		dtmail
  HELP_TOPIC		FPOnItemMail
  HELP_VOLUME		FPanel
  DELETE		True
}

CONTROL Blank1
{
  TYPE			blank
  CONTAINER_NAME	Top
  CONTAINER_TYPE	BOX
  POSITION_HINTS	6
  ICON			Fpblank
  HELP_TOPIC		FPOnItemFrontPanel
  HELP_VOLUME		FPanel
}

CONTROL Blank2
{
  TYPE			blank
  CONTAINER_NAME	Top
  CONTAINER_TYPE	BOX
  POSITION_HINTS	8
  ICON			Fpblank
  HELP_TOPIC		FPOnItemFrontPanel
  HELP_VOLUME		FPanel
}



CONTROL Printer
{
  TYPE			icon
  CONTAINER_NAME	Top
  CONTAINER_TYPE	BOX
  POSITION_HINTS	9
  LABEL			Default
  ICON			Fpprnt
  PUSH_ACTION		DtPrint
  DROP_ACTION		DtPrint
  DROP_ANIMATION	PrinterDrop
  HELP_TOPIC		FPOnItemPrinter
  HELP_VOLUME		FPanel
  DELETE		True
}


CONTROL	Xterm
{
   TYPE	file
   CONTAINER_NAME	Top
   CONTAINER_TYPE	BOX
   POSITION_HINTS	9
   ICON	Dtterm
   FILE_NAME	/usr/dt/appconfig/appmanager/C/Desktop_Tools/Xterm
   HELP_STRING	The Xterm action starts an 'xterm' terminal emulator.
}


CONTROL Style
{
  TYPE			icon
  CONTAINER_NAME	Top
  CONTAINER_TYPE	BOX
  POSITION_HINTS	10
  LABEL                 Desktop Style
  ICON                  Fpstyle
  PUSH_ACTION		Dtstyle
  PUSH_RECALL		true
  CLIENT_NAME		dtstyle
  HELP_TOPIC		FPOnItemStyle
  HELP_VOLUME		FPanel
  DELETE		True
}


CONTROL Applications
{
  TYPE			icon
  CONTAINER_NAME	Top
  CONTAINER_TYPE	BOX
  POSITION_HINTS	11
  ICON			Fpapps
  LABEL			Applications
  PUSH_ACTION		Dtappmgr
  HELP_TOPIC		FPOnItemAppMgr
  HELP_VOLUME		FPanel
  DELETE		True
}


CONTROL Help
{
  TYPE			icon
  CONTAINER_NAME	Top
  CONTAINER_TYPE	BOX
  POSITION_HINTS	12
  ICON			Fphelp
  LABEL			Help Manager
  PUSH_ACTION		Dthelpview
  DROP_ACTION		Dthelpview
  HELP_TOPIC		FPOnItemHelpMgr
  HELP_VOLUME		FPanel
}


CONTROL Trash
{
  TYPE			icon
  CONTAINER_NAME	Top
  CONTAINER_TYPE	BOX
  POSITION_HINTS	13
  ICON			Fptrsh
  LABEL			Trash
  ALTERNATE_ICON	Fptrsh7
  FILE_NAME		$HOME/.dt/Trash/.trashinfo
  MONITOR_TYPE		file
  PUSH_ACTION		Dttrash
  DROP_ACTION		Dttrash
  DROP_ANIMATION	TrashDrop
  HELP_TOPIC		FPOnItemTrash
  HELP_VOLUME		FPanel
  DELETE		True
}


SWITCH           Switch
{
  CONTAINER_NAME	Top
  POSITION_HINTS	7
  NUMBER_OF_ROWS	2
  HELP_TOPIC		FPOnItemSwitch
  HELP_VOLUME		FPanel
}



CONTROL Lock
{
  TYPE			icon
  CONTAINER_NAME	Switch
  CONTAINER_TYPE	SWITCH
  POSITION_HINTS	1
  ICON			Fplock
  LABEL			Lock
  PUSH_ACTION		LockDisplay
  HELP_TOPIC		FPOnItemLock
  HELP_VOLUME		FPanel
}


CONTROL Busy
{
  TYPE			busy
  CONTAINER_NAME	Switch
  CONTAINER_TYPE	SWITCH
  POSITION_HINTS	2
  ICON			Fplite
  ALTERNATE_ICON	FpliteY
  LABEL			Busy
  HELP_TOPIC		FPOnItemBusy
  HELP_VOLUME		FPanel
}


CONTROL Blank
{
  TYPE			blank
  CONTAINER_NAME	Switch
  CONTAINER_TYPE	SWITCH
  POSITION_HINTS	3
  ICON			FpblnkS
  HELP_TOPIC		FPOnItemSwitch
  HELP_VOLUME		FPanel
}


CONTROL Exit
{
  TYPE			icon
  CONTAINER_NAME	Switch
  CONTAINER_TYPE	SWITCH
  POSITION_HINTS	4
  ICON			Fpexit
  LABEL			Exit
  PUSH_ACTION		ExitSession
  HELP_TOPIC		FPOnItemExit
  HELP_VOLUME		FPanel
}



SUBPANEL Home
{
  CONTAINER_NAME	Home
  TITLE			Folders
}

CONTROL PersonalBookmarks
{
  TYPE			icon
  CONTAINER_NAME	Home
  CONTAINER_TYPE	SUBPANEL
  ICON			SDturlfile
  LABEL			Personal Bookmarks
  PUSH_ACTION		SDtPersonalBookmarks
  DROP_ACTION		SDtPersonalBookmarksAppend
  HELP_TOPIC		FPOnItemHome
  HELP_VOLUME		FPanel
}

CONTROL OpenFloppy
{
  TYPE			icon
  CONTAINER_NAME	Home
  CONTAINER_TYPE	SUBPANEL
  ICON			SDtRM.ofp
  LABEL			Open Floppy
  PUSH_ACTION		OpenFloppy
  HELP_TOPIC		FPOnItemHome
  HELP_VOLUME		FPanel
}

CONTROL OpenCD-ROM
{
  TYPE			icon
  CONTAINER_NAME	Home
  CONTAINER_TYPE	SUBPANEL
  ICON			SDtRM.ocd
  LABEL			Open CD-ROM
  PUSH_ACTION		OpenCD-ROM
  HELP_TOPIC		FPOnItemHome
  HELP_VOLUME		FPanel
}



SUBPANEL PersAppsSubpanel
{
  CONTAINER_NAME	Xterm
  TITLE			Personal Applications
}


CONTROL Term
{
  TYPE			icon
  CONTAINER_NAME	PersAppsSubpanel
  CONTAINER_TYPE	SUBPANEL
  POSITION_HINTS	1
  ICON			Fpterm
  LABEL			Terminal
  PUSH_ACTION		Terminal
  HELP_TOPIC		FPOnItemTerm
  HELP_VOLUME		FPanel
}



CONTROL WebBrowser
{
  TYPE			icon
  CONTAINER_NAME	PersAppsSubpanel
  CONTAINER_TYPE	SUBPANEL
  POSITION_HINTS	2
  ICON			SDtwebbr
  LABEL			Web Browser
  PUSH_ACTION		WebBrowser
  DROP_ACTION		WebBrowser
  HELP_TOPIC		TourSubpanels
  HELP_VOLUME		FPanel
}


CONTROL Style
{
  TYPE			icon
  CONTAINER_NAME	PersAppsSubpanel
  CONTAINER_TYPE	SUBPANEL
  POSITION_HINTS	3
  LABEL                 Desktop Style
  ICON                  Fpstyle
  PUSH_ACTION		Dtstyle
  PUSH_RECALL		true
  CLIENT_NAME		dtstyle
  HELP_TOPIC		FPOnItemStyle
  HELP_VOLUME		FPanel
}

CONTROL	Emacs
{
  TYPE	icon
  CONTAINER_NAME	PersAppsSubpanel
  CONTAINER_TYPE	SUBPANEL
  POSITION_HINTS	4
  LABEL			Emacs
  ICON	Fppenpd
  PUSH_ACTION		Emacs
  PUSH_RECALL		True
  CLIENT_NAME		emacs
  HELP_STRING		The Emacs action starts the Emacs editor.
}


SUBPANEL PersPrintersSubpanel
{
   CONTAINER_NAME	Printer
   TITLE		Personal Printers
}

CONTROL PrintManager
{
  TYPE			icon
  CONTAINER_NAME	PersPrintersSubpanel
  CONTAINER_TYPE	SUBPANEL
  POSITION_HINTS	1
  ICON			FpPrtmg
  LABEL			Print Manager
  PUSH_ACTION		DtPrintManager
  DROP_ACTION		DtPrint
  DROP_ANIMATION	PrinterDrop
  HELP_TOPIC		_hometopic
  HELP_VOLUME		Printmgr
}


SUBPANEL Applications
{
   CONTAINER_NAME	Applications
   TITLE		Applications
}

set DtAppMgrFolder=/var/dt/appconfig/appmanager/$DTUSERSESSION

CONTROL Desktop_Apps
{
  TYPE			file
  CONTAINER_NAME	Applications
  CONTAINER_TYPE	SUBPANEL
  POSITION_HINTS	1
  ICON			Dtadskt
  LABEL			Desktop_Apps
  FILE_NAME		${DtAppMgrFolder}/Desktop_Apps
  HELP_TOPIC		TourSubpanels
  HELP_VOLUME		FPanel
}

CONTROL Desktop_Controls
{
  TYPE			file
  CONTAINER_NAME	Applications
  CONTAINER_TYPE	SUBPANEL
  POSITION_HINTS	2
  ICON			SDtactrls
  LABEL			Desktop_Controls
  FILE_NAME		${DtAppMgrFolder}/Desktop_Controls
  HELP_TOPIC		TourSubpanels
  HELP_VOLUME		FPanel
}

CONTROL Information
{
  TYPE			file
  CONTAINER_NAME	Applications
  CONTAINER_TYPE	SUBPANEL
  POSITION_HINTS	3
  ICON			Dtainfo
  LABEL			Information
  FILE_NAME		${DtAppMgrFolder}/Information
  HELP_TOPIC		TourSubpanels
  HELP_VOLUME		FPanel
}


SUBPANEL HelpSubpanel
{
  CONTAINER_NAME	Help
  TITLE			Help
}

CONTROL HelpOverview
{
  TYPE			icon
  CONTAINER_NAME	HelpSubpanel
  CONTAINER_TYPE	SUBPANEL
  POSITION_HINTS	1
  ICON			Dthover
  LABEL			Desktop Introduction
  PUSH_ACTION		OpenDtIntro
  HELP_TOPIC		FPOnItemDtIntro
  HELP_VOLUME		FPanel
}

CONTROL FPHelp
{
  TYPE			icon
  CONTAINER_NAME	HelpSubpanel
  CONTAINER_TYPE	SUBPANEL
  POSITION_HINTS	2
  ICON			Fpfphlp
  LABEL			Front Panel Help
  PUSH_ACTION		FPHelp
  HELP_TOPIC		FPOnItemFPHelp
  HELP_VOLUME		FPanel
}

#
# OnItem help uses a pseudo push action FPOnItemHelp.  Dtwm is looking for
# an exact match on this push action string.  Do not localize this push action.
#

CONTROL OnItem
{
  TYPE			icon
  CONTAINER_NAME	HelpSubpanel
  CONTAINER_TYPE	SUBPANEL
  POSITION_HINTS	3
  ICON			DthonFP
  LABEL			On Item Help
  PUSH_ACTION		FPOnItemHelp
  HELP_TOPIC		FPOnItemOnItem
  HELP_VOLUME		FPanel
}


CONTROL AnswerBook2
{
  TYPE			icon
  CONTAINER_NAME	HelpSubpanel
  CONTAINER_TYPE	SUBPANEL
  POSITION_HINTS	4
  ICON			SDtab2
  LABEL			AnswerBook2 
  PUSH_ACTION		SDTab2
  HELP_TOPIC		TourSubpanels
  HELP_VOLUME		FPanel
}


ANIMATION TrashDrop
{
   ANIMATION	Fptrsh1		100
   ANIMATION	Fptrsh2		200
   ANIMATION	Fptrsh3		100
   ANIMATION	Fptrsh4
   ANIMATION	Fptrsh5		800
   ANIMATION	Fptrsh6		200
   ANIMATION	Fptrsh7
   ANIMATION	Fptrsh
}

ANIMATION PrinterDrop
{
   ANIMATION	Fpprnt1		100
   ANIMATION	Fpprnt2		100
   ANIMATION	Fpprnt3		100
   ANIMATION	Fpprnt4		100
   ANIMATION	Fpprnt5		100
   ANIMATION	Fpprnt6		100
   ANIMATION	Fpprnt7		100
   ANIMATION	Fpprnt8		100
   ANIMATION	Fpprnt9		100
   ANIMATION	FpprntA		100
   ANIMATION	FpprntB		800
}

