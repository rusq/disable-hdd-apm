;
; Disable HDD APM tray version. Stops your drive from clicking (excessive drive head parking)
;
;    Copyright (C) 2012 Gilyazov Rustam <gilyazov@live.com>
;
;    This program is free software: you can redistribute it and/or modify
;    it under the terms of the GNU General Public License as published by
;    the Free Software Foundation, either version 3 of the License, or
;    (at your option) any later version.
;
;    This program is distributed in the hope that it will be useful,
;    but WITHOUT ANY WARRANTY; without even the implied warranty of
;    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;    GNU General Public License for more details.

;    You should have received a copy of the GNU General Public License
;    along with this program.  If not, see <http://www.gnu.org/licenses/>.


STRVERSION equ '2.0 beta'

;How often to check for LCC changes (in minutes)
AUTOTIMER = 5

; Different defines
struct GUID
       Data1 dd ?
       Data2 dw ?
       Data3 dw ?
       Data4 db 8 dup(?)
ends

ANYSIZE_ARRAY = 1


struct SID_IDENTIFIER_AUTHORITY
       Value db 6 dup(?)
ends

struct SID
       Revision db ?
       SubAuthorityCount db ?
       IdentifierAuthority SID_IDENTIFIER_AUTHORITY
       SubAuthority dd ANYSIZE_ARRAY dup(?)
ends

;Size of the unicode buffer (for wszBuffer)
UNICODE_BUF_SIZE equ 2000h

;NotifyIcon constants
NIIF_NONE = 0
NIIF_INFO = 1
NIIF_WARNING = 2
NIIF_ERROR = 3
NIIF_USER = 4
NIIF_ICON_MASK = $F
NIIF_NOSOUND = $10
NIIF_LARGE_ICON = $20
NIIF_RESPECT_QUIET_TIME = $80

NOTIFYICONDATAA_V1_SIZE = 88
NOTIFYICONDATAW_V1_SIZE = 152
NOTIFYICONDATAA_V2_SIZE = 488
NOTIFYICONDATAW_V2_SIZE = 936
NOTIFYICONDATAA_V3_SIZE = 504
NOTIFYICONDATAW_V3_SIZE = 952

struct NOTIFYICONDATA2
       cbSize dd ?
       hWnd dd ?
       uID dd ?
       uFlags dd ?
       uCallbackMessage dd ?
       hIcon dd ?
       szTip du 128 dup(?)
       dwState dd ?
       dwStateMask dd ?
       szInfo du 256 dup(?)
       union
	   uTimeout dd ?
	   uVersion dd ?
       ends
       szInfoTitle du 64 dup(?)
       dwInfoFlags dd ?
       guidItem GUID
       hBalloonIcon dd ?
ends
