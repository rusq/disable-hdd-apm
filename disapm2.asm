;
; Disable HDD APM tray version. Stops your drive from clicking (excessive drive head parking)
;
;    Copyright (C) 2012 Gilyazov Rustam
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

; Reference:
; http://stackoverflow.com/questions/7008738/how-get-hdd-current-load-info-in-windows
; http://msdn.microsoft.com/en-us/library/windows/desktop/aa363216(v=vs.85).aspx
; Power management:
; http://msdn.microsoft.com/en-us/library/windows/desktop/aa373223(v=vs.85).aspx
; http://msdn.microsoft.com/en-us/library/windows/desktop/aa373163(v=vs.85).aspx

format PE GUI 5.0

include 'win32w.inc'
include 'disapm2.inc'
include 'winioctl.inc'
include 'ata.inc'

section '.text' code readable executable

entry $
	invoke	GetModuleHandle,0
	mov	[hInstance],eax

	invoke	CreateMutex,0,FALSE,_mutex		;Creating mutex
	mov	[hMutex],eax
	invoke	GetLastError
	cmp	eax,ERROR_ALREADY_EXISTS		;or already exists
	jne	@f

	invoke	MessageBox,0,_errAR,0,MB_ICONERROR+MB_OK  ;say ho
	jmp	end_loop

  @@:
	cmp	[hMutex],0
	jz	error

	;checking if ATA drives are available
	call	getPhysDrives
	or	eax,eax
	jnz	@f
	invoke	MessageBox,0,_NoATA,_EpicFail,MB_OK+MB_ICONWARNING
	jmp	end_loop

  @@:
	invoke	CreateDialogParam,[hInstance],IDD_MAIN,HWND_DESKTOP,WindowProc,0
	test	eax,eax
	jz	error

  msg_loop:
	invoke	GetMessage,msg,NULL,0,0
	cmp	eax,1
	jb	end_loop
	jne	msg_loop
	invoke	IsDialogMessage,[hWnd],msg
	jne	msg_loop
	invoke	TranslateMessage,msg
	invoke	DispatchMessage,msg
	jmp	msg_loop

  error:
	invoke	MessageBox,NULL,_error,NULL,MB_ICONERROR+MB_OK

  end_loop:
	invoke	ExitProcess,[msg.wParam]

proc WindowProc uses ebx esi edi, hwnd,wmsg,wparam,lparam
	mov	eax,[wmsg]
	cmp	eax,WM_INITDIALOG
	je	.wminitdialog
	cmp	eax,WM_COMMAND
	je	.wmcommand
	cmp	eax,WM_MYMESSAGE
	je	.wmmymessage
	cmp	eax,WM_TIMER
	je	.wmtimer
	cmp	eax,WM_SYSCOMMAND
	je	.wmsyscommand
	cmp	eax,WM_CLOSE
	je	.tray
	cmp	eax,WM_DESTROY
	je	.wmdestroy
	xor	eax,eax
	jmp	.finish
  
  .wminitdialog:
	invoke	LoadIcon,[hInstance],IDI_MAIN
	mov	[hIcon],eax
	invoke	SendMessage,[hwnd],WM_SETICON,ICON_SMALL,eax

	;Adding "About" to system menu
	invoke	GetSystemMenu,[hwnd],0
	push	eax
	invoke	AppendMenu,eax,MF_SEPARATOR,0,0
	pop	eax
	invoke	AppendMenu,eax,MF_STRING,IDC_ABOUT,_About


	mov	eax,[hwnd]
	mov	[hWnd],eax

	; Adding drives to combobox
	stdcall addDrivesToCombo
	or	eax,eax
	jnz	@f
	invoke	MessageBox,0,_NotSupp,_EpicFail,MB_OK+MB_ICONWARNING
	jmp	.wmdestroy
     @@:
	call	tray

	;Registering for power messages
	;invoke RegisterPowerSettingsNotification,[hwnd],,DEVICE_NOTIFY_WINDOW_HANDLE

	jmp    .processed
	;jmp     .switch_auto
  .wmtimer:
	call	timerHandler
	jmp	.processed
  .wmcommand:
	mov	eax,[wparam]
	cmp	eax,BN_CLICKED shl 16+IDC_EXIT
	je	.wmdestroy
	cmp	eax,BN_CLICKED shl 16+IDC_HIDE
	je	.tray
	cmp	eax,BN_CLICKED shl 16+IDM_MENU_RESTORE
	je	.dbl
	cmp	eax,BN_CLICKED shl 16+IDM_MENU_AUTO
	je	.switch_auto
	cmp	eax,BN_CLICKED shl 16+IDM_MENU_NOTIF
	je	.switch_notif
	cmp	eax,BN_CLICKED shl 16+IDC_AUTO
	je	.auto
	cmp	eax,BN_CLICKED shl 16+IDC_DISABLENOW
	je	.disablenow
	cmp	eax,CBN_SELCHANGE shl 16 + IDC_DRIVE
	je	.combobreaker
	jmp	.processed

    .tray:
	call	tray
	invoke	ShowWindow,[hWnd],FALSE
	stdcall showtip,_baloon1,_baloon1_ttl,300,NIIF_INFO

	jmp	.processed

    .switch_auto:
	stdcall getstate,IDC_AUTO
	jnc	@f
	invoke	SendDlgItemMessage,[hwnd],IDC_AUTO,BM_SETCHECK,BST_UNCHECKED,0
	jmp	.auto
      @@:
	invoke	SendDlgItemMessage,[hwnd],IDC_AUTO,BM_SETCHECK,BST_CHECKED,0
    .auto:
	call	automode
	jmp	.processed

    .switch_notif:
	stdcall getstate,IDC_NONOTIF
	jnc	@f
	invoke	SendDlgItemMessage,[hwnd],IDC_NONOTIF,BM_SETCHECK,BST_UNCHECKED,0
	jmp	.processed
     @@:
	invoke	SendDlgItemMessage,[hwnd],IDC_NONOTIF,BM_SETCHECK,BST_CHECKED,0
	jmp	.processed

    .disablenow:
	call	disableapm
	jmp	.processed

    .combobreaker:
	call	combobreaker
	jmp	.processed

    .about:
	invoke	SetForegroundWindow,[hwnd]
	invoke	MessageBox,[hwnd],_AboutTxt,_About,MB_OK+MB_ICONINFORMATION
	jmp	.processed

  .wmsyscommand:
	cmp	[wparam],BN_CLICKED shl 16 + IDC_ABOUT
	je	.about
	jmp	.processed



  .wmmymessage:
	cmp	[lparam],WM_LBUTTONDBLCLK
	je	.dbl
	cmp	[lparam],WM_RBUTTONUP
	je	.rclick
	jmp	.processed
    .dbl:
	invoke	IsWindowVisible,[hwnd]
	xor	eax,1
	invoke	ShowWindow,[hwnd],eax
	jmp	.processed
    .rclick:
	stdcall traymenu,[hwnd]
	jmp	.processed

  .wmdestroy:
	cmp	[bNotify],TRUE
	jne	@f
	;destroying the icon
	mov	[nid.cbSize],sizeof.NOTIFYICONDATA2
	mov	eax,[hwnd]
	mov	[nid.hWnd],eax
	mov	[nid.uID],IDT_ICON
	invoke	Shell_NotifyIcon,NIM_DELETE,nid
    @@:
	invoke	EndDialog,[hwnd],0
	invoke	CloseHandle,[hMutex]
	invoke	PostQuitMessage,0
  .processed:
	xor	eax,eax
  .finish:
	ret
endp

;Timer handler procedure
proc timerHandler
	cmp	[lastLCC],0			;if lastLCC is not set
	je	.finish 			;nothing to do.
	call	getSelectedDrive
	cmp	eax,CB_ERR
	jz	.finish
	stdcall getLoadCycleCount,szBuffer	;getting the new LCC value
	cmp	eax,[lastLCC]			;if new LCC greater than last measure, then activate our magic
	jna	.finish
	mov	[lastLCC],eax
	call	disableapm
	cinvoke wsprintf,wszBuffer,_LCCFmt,[lastLCC]
	invoke	SendDlgItemMessage,[hWnd],IDC_CURLCC,WM_SETTEXT,0,wszBuffer

  .finish:
	ret
endp

;Show LCC on combo selection change (or on call)
proc combobreaker
	call	getSelectedDrive
	cmp	eax,CB_ERR
	jz	.finish
	stdcall getLoadCycleCount,szBuffer
	cmp	eax,-1
	jnz	@f
	invoke	SendDlgItemMessage,[hWnd],IDC_CURLCC,WM_SETTEXT,0,_LCCNa
	;switch auto to off and disable control
	invoke	KillTimer,[hWnd],IDT_TIMER			;switching off timer and auto mode,
	invoke	GetDlgItem,[hWnd],IDC_AUTO			;when unable to trace the LCC data
	push	eax
	invoke	SendMessage,eax,BM_SETCHECK,BST_UNCHECKED,0
	pop	eax
	invoke	EnableWindow,eax,FALSE
	invoke	GetDlgItem,[hWnd],IDC_DISABLENOW
	invoke	EnableWindow,eax,FALSE
	mov	[lastLCC],0

	jmp	.finish
     @@:
	mov	[lastLCC],eax			;Saving current LCC value, to trace the changes
	cinvoke wsprintf,wszBuffer,_LCCFmt,eax
	invoke	SendDlgItemMessage,[hWnd],IDC_CURLCC,WM_SETTEXT,0,wszBuffer
	invoke	GetDlgItem,[hWnd],IDC_AUTO
	invoke	EnableWindow,eax,TRUE
	invoke	GetDlgItem,[hWnd],IDC_DISABLENOW
	invoke	EnableWindow,eax,TRUE

   .finish:
	ret
endp

; Adds an icon to tray
proc tray uses edi esi
	cmp	[bNotify],TRUE
	je	.hide
	mov	[nid.cbSize],NOTIFYICONDATAW_V2_SIZE
	mov	eax,[hWnd]
	mov	[nid.hWnd],eax
	mov	[nid.uID],IDT_ICON
	;mov    [nid.uVersion],NOTIFYICON_VERSION
	mov	[nid.uCallbackMessage],WM_MYMESSAGE
	invoke	LoadIcon,[hInstance],IDI_MAIN
	mov	[nid.hIcon],eax
	mov	esi,_tip
	mov	edi,nid.szTip
	mov	ecx,_tip_sz
	rep	movsb
	mov	[nid.uFlags],NIF_MESSAGE or NIF_ICON or NIF_TIP

	invoke	Shell_NotifyIcon,NIM_ADD,nid
	mov	[bNotify],eax

  .hide:
	ret
endp

; Shows tray baloon:
; lpwszInfo     -       in: pointer to Wide Asciiz string - Information message
; lpwszInfoTitle-       in: pointer to Wide Asciiz string - Information title
; dwTimeout     -       in: Timeout in milliseconds
; icn           -       in: Icon to use (from the standard ones)
proc showtip uses edi esi, lpwszInfo,lpwszInfoTitle,dwTimeout,icn
	;check if icon in tray is present
	cmp	[bNotify],TRUE
	jne	.finish

	cmp	[icn],NIIF_ERROR	;important notifications are shown disregarding (NO_NOTIFICATIONS) check-box
	je	@f

	stdcall getstate,IDC_NONOTIF
	jc	.finish
  @@:
	;calculate string sizes:
	;szInfo:
	mov	esi,[lpwszInfo]
	stdcall strlenw,esi
	mov	edi,nid.szInfo
	rep	movsw
	xor	ax,ax
	stosw

	mov	esi,[lpwszInfoTitle]
	stdcall strlenw,esi
	mov	edi,nid.szInfoTitle
	rep	movsw
	xor	ax,ax
	stosw

	mov	[nid.cbSize],NOTIFYICONDATAW_V2_SIZE
	mov	eax,[hWnd]
	mov	[nid.hWnd],eax
	mov	[nid.uID],IDT_ICON
	mov	[nid.uFlags],NIF_INFO
	mov	eax,[icn]
	mov	[nid.dwInfoFlags],eax
	mov	eax,[dwTimeout]

	invoke	Shell_NotifyIcon,NIM_MODIFY,nid

  .finish:
	ret
endp

; Shows tray menu on mouse Right-click
proc traymenu uses edi,hwnd:DWORD
  local hMenu:DWORD,pt:POINT,mii:MENUITEMINFO
  ;http://www.codeproject.com/Articles/4768/Basic-use-of-Shell_NotifyIcon-in-Win32
	lea	eax,[pt]
	invoke	GetCursorPos,eax
	;clearing MENUITEMINFO structure
	lea	edi,[mii]
	mov	ecx,sizeof.MENUITEMINFO
	xor	al,al
	rep	stosb

	invoke	SetForegroundWindow,[hwnd]
	invoke	CreatePopupMenu
	or	eax,eax
	jz	.finish
	mov	[hMenu],eax

	;first menu item (restore/hide)
	mov	[mii.cbSize],sizeof.MENUITEMINFO
	mov	[mii.fMask],MIIM_STATE+MIIM_STRING+MIIM_ID
	mov	[mii.fState],MFS_DEFAULT
	mov	[mii.wID],IDM_MENU_RESTORE and 0FFFFh

	invoke	IsWindowVisible,[hwnd]
	or	eax,eax
	jz	@f
	mov	[mii.dwTypeData],_menu2
	jmp	.add1
      @@:
	mov	[mii.dwTypeData],_menu1
      .add1:
	lea	edi,[mii]
	invoke	InsertMenuItem,[hMenu],-1,TRUE,edi

	;second menu item (autodisable)
	stdcall getstate,IDC_AUTO
	jnc	@f
	mov	[mii.fState],MFS_CHECKED
	jmp	.add2
      @@:
	mov	[mii.fState],MFS_UNCHECKED
      .add2:
	mov	[mii.wID],IDM_MENU_AUTO
	mov	[mii.dwTypeData],_menu3
	invoke	InsertMenuItem,[hMenu],-1,TRUE,edi

	stdcall getstate,IDC_NONOTIF
	jnc	@f
	mov	[mii.fState],MFS_CHECKED
	jmp	.add3
      @@:
	mov	[mii.fState],MFS_UNCHECKED

      .add3:
	mov	[mii.wID],IDM_MENU_NOTIF
	mov	[mii.dwTypeData],_menu4
	invoke	InsertMenuItem,[hMenu],-1,TRUE,edi

	;separator
	mov	[mii.fMask],MIIM_FTYPE
	mov	[mii.fType],MF_SEPARATOR
	invoke	InsertMenuItem,[hMenu],-1,TRUE,edi

	;disable now
	mov	[mii.fMask],MIIM_STRING+MIIM_ID
	mov	[mii.wID],IDC_DISABLENOW
	mov	[mii.dwTypeData],_menu5
	invoke	InsertMenuItem,[hMenu],-1,TRUE,edi


	;separator
	mov	[mii.fMask],MIIM_FTYPE
	mov	[mii.fType],MF_SEPARATOR
	invoke	InsertMenuItem,[hMenu],-1,TRUE,edi


	;exit
	mov	[mii.fMask],MIIM_STRING+MIIM_ID
	mov	[mii.wID],IDC_EXIT
	mov	[mii.dwTypeData],_menu6
	invoke	InsertMenuItem,[hMenu],-1,TRUE,edi

	;http://msdn.microsoft.com/en-us/library/windows/desktop/ms648002(v=vs.85).aspx
	invoke	TrackPopupMenu,[hMenu],TPM_BOTTOMALIGN,[pt.x],[pt.y],0,[hwnd],NULL
	invoke	DestroyMenu,[hMenu]
  .finish:
	ret
endp

; Obsolete, uses menu from resources. Isn't used in the code.
proc traymenu1 hwnd:DWORD
  local hMenu:DWORD,pt:POINT
  ;http://www.codeproject.com/Articles/4768/Basic-use-of-Shell_NotifyIcon-in-Win32
	lea	eax,[pt]
	invoke	GetCursorPos,eax
	invoke	SetForegroundWindow,[hwnd]
	invoke	LoadMenu,[hInstance],IDM_MENU
	mov	[hMenu],eax
	invoke	GetSubMenu,eax,0
	;http://msdn.microsoft.com/en-us/library/windows/desktop/ms648002(v=vs.85).aspx
	invoke	TrackPopupMenu,eax,TPM_BOTTOMALIGN,[pt.x],[pt.y],0,[hwnd],NULL
	invoke	DestroyMenu,[hMenu]
  .finish:
	ret
endp

; Strlen for Wide-char ASCIIZ strings
;
; Parameters:
; lpwszString   -       in: pointer to Wide Asciiz string
;
; Returns:
;       eax - number of characters
proc strlenw uses edi, lpwszString:DWORD
	mov	edi,[lpwszString]
	or	ecx,-1
	xor	eax,eax
	repne	scasw
	neg	ecx
	sub	ecx,2
	mov	eax,ecx
	ret
endp

; Automatic mode manipulation
proc automode
	stdcall getstate,IDC_AUTO
	jnc	.autooff
	;
	;turning on the timer
	;
	call	getSelectedDrive
	cmp	eax,CB_ERR
	jz	.finish

	stdcall getLoadCycleCount,szBuffer
	mov	[lastLCC],eax						;saving current LCC value (for comparison)

	invoke	SetTimer,[hWnd],IDT_TIMER,AUTOTIMER*1000*60,NULL	;setting timer to AUTOTIMER minutes

	jmp	.finish
    .autooff:
	; Switching off automatic mode
	invoke	KillTimer,[hWnd],IDT_TIMER	;killing timer
	mov	[lastLCC],0			;resetting last lcc value

  .finish:
	ret
endp

; Gets the current selection in Drive combobox
;
; Return:
;       eax - drive index
;       szBuffer - contains the drive path in form \\.\PhysicalDrive0
proc getSelectedDrive
	invoke	SendDlgItemMessage,[hWnd],IDC_DRIVE,CB_GETCURSEL,0,0
	cmp	eax,CB_ERR
	jz	.finish
	push	eax
	invoke	SendDlgItemMessage,[hWnd],IDC_DRIVE,CB_GETITEMDATA,eax,0
	cinvoke wsprintf,szBuffer,_driveFmt,eax
	pop	eax
  .finish:
	ret
endp

;
; Disable apm for selected in combobox device
;
proc disableapm
  local curItem:DWORD,hDrive:DWORD
	call	getSelectedDrive
	cmp	eax,CB_ERR
	jz	.finish
	mov	[curItem],eax
	invoke	CreateFile,szBuffer,GENERIC_READ + GENERIC_WRITE, FILE_SHARE_READ+FILE_SHARE_WRITE,0,OPEN_EXISTING,0,0
	cmp	eax,-1
	jne	@f
	stdcall showtip,_ErrorTxt,_Error,3000,NIIF_ERROR
	jmp	.finish

     @@:
	mov	[hDrive],eax

	mov	[inbuffer.Length],sizeof.ATA_PASS_THROUGH_EX
	mov	[inbuffer.AtaFlags],ATA_FLAGS_DATA_IN
	mov	[inbuffer.DataTransferLength],512
	mov	[inbuffer.DataBufferOffset],sizeof.ATA_PASS_THROUGH_EX
	mov	byte[inbuffer.ucDataBuf],$CF				;magic number
	mov	[inbuffer.TimeOutValue],1
	mov	[inbuffer.CurrentTaskFile.bCommandReg],WIN_SETFEATURES
	mov	[inbuffer.CurrentTaskFile.bFeaturesReg],SETFEATURES_DIS_APM
	mov	[inbuffer.CurrentTaskFile.bSectorCountReg],0

	invoke	DeviceIoControl,\
		[hDrive],\
		IOCTL_ATA_PASS_THROUGH,\
		inbuffer,\
		sizeof.ATA_PASS_THROUGH_EX_WITH_BUFFERS,\
		inbuffer,\
		sizeof.ATA_PASS_THROUGH_EX_WITH_BUFFERS,\
		bytes_count,\
		0

	invoke	CloseHandle,[hDrive]
	invoke	SendDlgItemMessage,[hWnd],IDC_DRIVE,CB_GETLBTEXT,[curItem],szBuffer
	cinvoke wsprintf,wszBuffer,_OkText,szBuffer
	stdcall showtip,wszBuffer,_Ok,3000,NIIF_INFO

  .finish:
	ret
endp


; returns the checkbox state in carry-flag.
; Carry set - checked
; Carry not set - unchecked

proc getstate	dlgItemId
	invoke	SendDlgItemMessage,[hWnd],[dlgItemId],BM_GETCHECK,0,0
	cmp	eax,BST_CHECKED
	jne	.not_checked
	stc
	jmp	.finish
  .not_checked:
	clc
  .finish:
	ret
endp

;Brute-force detection of physical drives
proc getPhysDrives uses esi
	xor	esi,esi
  .next_drive:
	cinvoke wsprintf,szBuffer,_driveFmt,esi
	invoke	CreateFile,szBuffer,GENERIC_READ + GENERIC_WRITE, FILE_SHARE_READ+FILE_SHARE_WRITE,0,OPEN_EXISTING,0,0
	cmp	eax,-1
	je	.finish
	invoke	CloseHandle,eax
	mov	byte[drivearray+esi],1
	inc	esi
	cmp	esi,64
	je	.finish
	jmp	.next_drive
  .finish:
	mov	eax,esi
	ret
endp

; Add detected drives to combo box
proc addDrivesToCombo uses ebx esi
	xor	esi,esi

     .next:
	cmp	byte[drivearray+esi],0
	jz	.skip
     .add:
	cinvoke wsprintf,szBuffer,_driveFmt,esi
	stdcall getPhysDriveInfo,szBuffer
	or	eax, eax
	jz	.skip
	invoke	MultiByteToWideChar,437,0,drivemodel,-1,wszBuffer,UNICODE_BUF_SIZE
	invoke	SendDlgItemMessage,[hWnd],IDC_DRIVE,CB_ADDSTRING,0,wszBuffer
	invoke	SendDlgItemMessage,[hWnd],IDC_DRIVE,CB_SETITEMDATA,eax,esi	;add index of the drive for later use
     .skip:
	inc	esi
	cmp	esi,64
	jz	.finish
	jmp	.next

  .finish:
	invoke	SendDlgItemMessage,[hWnd],IDC_DRIVE,CB_GETCOUNT,0,0	;check if there are any records
	or	eax,eax 						;if not, exit (eax=0)
	jz	.ret
	invoke	SendDlgItemMessage,[hWnd],IDC_DRIVE,CB_SETCURSEL,0,0	;selecting the first item
	call	combobreaker						;to set LCC value
	mov	eax,TRUE
	jmp	.ret

; .error:
;       xor     eax,eax
  .ret:
	ret

endp

; returns the drive model to drivemodel variable
; eax = TRUE on success, FALSE on error

proc getPhysDriveInfo uses esi edi, lpwszDrive:DWORD
  local hDrive:DWORD,bytes_count:DWORD,err:DWORD
	mov	[err],1
	invoke	CreateFile,[lpwszDrive],GENERIC_READ + GENERIC_WRITE, FILE_SHARE_READ+FILE_SHARE_WRITE,0,OPEN_EXISTING,0,0
	cmp	eax,-1
	jnz	@f
	mov	[err],0
	jmp	.close
     @@:
	mov	[hDrive],eax
	mov	[atabuffer.Length],sizeof.ATA_PASS_THROUGH_EX
	mov	[atabuffer.DataBufferOffset],sizeof.ATA_PASS_THROUGH_EX
	mov	[atabuffer.DataTransferLength],512
	mov	[atabuffer.AtaFlags],ATA_FLAGS_DATA_IN
	mov	[atabuffer.CurrentTaskFile.bCommandReg],0ECh		;identify command (doesn't work on VMWARE SCSI drives)
	mov	[atabuffer.CurrentTaskFile.bSectorCountReg],1
	mov	[atabuffer.TimeOutValue],2
	lea	eax,[bytes_count]
	invoke	DeviceIoControl,\
		[hDrive],\
		IOCTL_ATA_PASS_THROUGH,\
		atabuffer,\
		sizeof.ATA_PASS_THROUGH_EX_WITH_BUFFERS,\
		atabuffer,\
		sizeof.ATA_PASS_THROUGH_EX_WITH_BUFFERS,\
		eax,\
		0
	or	eax,eax
	jnz	.getinfo
	mov	[err],FALSE
	jmp	.close

  .getinfo:
	stdcall ataStrToAscii,\
		drivemodel,\
		atabuffer+sizeof.ATA_PASS_THROUGH_EX+IDENTIFY_DEVICE_DATA.ModelNumber,\
		40
	stdcall checkSmart,[hDrive]		;check if drive supports smart
	or	eax,eax
	jnz	.close
	mov	[err],FALSE
  .close:
	invoke	CloseHandle,[hDrive]
	mov	eax,[err]
	ret
endp

proc ataStrToAscii uses esi edi, lpBuffer:DWORD,lpataStr:DWORD,dwLen:DWORD
	mov	esi,[lpataStr]
	mov	edi,[lpBuffer]
	mov	ecx,[dwLen]
	shr	ecx,1
      @@:
	lodsw
	cmp	ax,2020h
	je	.endstring
	xchg	ah,al
	stosw
	dec	ecx
	jnz	@b
  .endstring:
	xor	al,al
	cmp	byte[edi-1],20h
	jne	@f
	dec	edi
      @@:
	stosb

	ret
endp

;TRUE - S.M.A.R.T. present, FALSE - not present.
proc checkSmart hDrive:DWORD
  local bytes_returned:DWORD

  CAP_SMART_CMD = 4

	lea	eax,[bytes_returned]
	invoke	DeviceIoControl,\
		[hDrive],\
		SMART_GET_VERSION,\
		0,\
		0,\
		gvp,\
		sizeof.GETVERSIONINPARAMS,\
		eax,\
		0
	or	eax,eax
	jz	.finish
	test	[gvp.fCapabilities],CAP_SMART_CMD		;bit is set when S.M.A.R.T. is supported
	jz	.notsup
	mov	eax,TRUE
  .finish:
	ret
  .notsup:
	xor	eax,eax
	jmp	.finish
endp

;In: lpwszDrive - drive
;Out: EAX-load cycle count, -1 on error
proc getLoadCycleCount uses ebx, lpwszDrive:DWORD
  local hDrive:DWORD,bytes_received:DWORD
; SMART_SEND_DRIVE_COMMAND: http://msdn.microsoft.com/en-us/library/windows/hardware/ff566206(v=vs.85).aspx
; SENDCMDINPARAMS structure: http://msdn.microsoft.com/en-us/library/windows/hardware/ff565401(v=vs.85).aspx
; SMART_RCV_DRIVE_DATA control code: http://msdn.microsoft.com/en-us/library/windows/hardware/ff566204(v=vs.85).aspx
	invoke	CreateFile,[lpwszDrive],GENERIC_READ + GENERIC_WRITE, FILE_SHARE_READ+FILE_SHARE_WRITE,0,OPEN_EXISTING,0,0
	cmp	eax,-1
	je	.return
	mov	[hDrive],eax
	mov	[scip.irDriveRegs.bCommandReg],SMART_CMD
	mov	[scip.irDriveRegs.bFeaturesReg],READ_ATTRIBUTES
	mov	[scip.irDriveRegs.bSectorNumberReg],1
	mov	[scip.irDriveRegs.bSectorCountReg],1
	mov	[scip.irDriveRegs.bCylLowReg],SMART_CYL_LOW
	mov	[scip.irDriveRegs.bCylHighReg],SMART_CYL_HI
	mov	[scip.irDriveRegs.bDriveHeadReg],DRIVE_HEAD_REG
	mov	[scip.cBufferSize],READ_ATTRIBUTE_BUFFER_SIZE

	lea	eax,[bytes_received]

	invoke	DeviceIoControl,\
		[hDrive],\
		SMART_RCV_DRIVE_DATA,\
		scip,\
		sizeof.SENDCMDINPARAMS-1,\
		scop,\
		sizeof.SENDCMDOUTPARAMS-1+READ_ATTRIBUTE_BUFFER_SIZE,\
		eax,\
		0
	invoke	CloseHandle,[hDrive]

	or	eax,-1			; if parameter won't be found, eax will stay -1
	mov	ebx,scopBuffer		; output buffer from DeviceIO Control
	virtual at ebx
		.si SMARTINFO
	end virtual
     @@:
	cmp	[.si.AttribIndex],0C1h				;LOAD/UNLOAD CYCLE COUNT
	je	@f
	add	ebx,sizeof.SMARTINFO
	cmp	ebx,scopBuffer+READ_ATTRIBUTE_BUFFER_SIZE
	ja	.return
	jmp	@b

    @@:
	mov	eax,[.si.Data]

  .return:
	ret
endp


section '.data' data readable writeable

  _tip	 TCHAR 'HDD APM disabler',0
  _tip_sz = $-_tip
  ;Notify area strings
  _baloon1_ttl	TCHAR 'HDD APM disabler is minimized',0
  _baloon1	TCHAR 'Running in background. Double-click this icon to restore.',0
  _Error	TCHAR 'Unable to disable APM',0
  _ErrorTxt	TCHAR 'Unable to disable APM, probably you don''t have administrator privileges.',0
  _Ok		TCHAR 'APM disabled',0
  _OkText	TCHAR 'APM for %s has been disabled',0
  ; Drive Stuff
  ;  _drive        TCHAR '\\.\PhysicalDrive0',0
  _driveFmt	TCHAR '\\.\PhysicalDrive%d',0
  ; MessageBoxes
  _NoATA	TCHAR 'No fixed drives found. Aborting.',0
  _NotSupp	TCHAR 'Drive(s) found but none are supported.',0
  _EpicFail	TCHAR 'Epic Fail',0
  _errAR	TCHAR 'Already running',0
  _error	TCHAR 'Startup failed.',0
  ; Other strings
  _LCCNa	TCHAR 'N/A',0		; LCC not available
  _LCCFmt	TCHAR '%d',0		; LCC format
  _About	TCHAR 'About...',0
  _AboutTxt	TCHAR 'HDD APM Disabler v.',STRVERSION,13,10,\
		      '(c) 2012 gilyazov@live.com',13,10,\
		      'Licensed under GPL',0
  ;registry entries
  _RegPath	TCHAR 'Software\GilyazovR\DisAPM',0

  ;menu items
  _menu1 TCHAR 'Restore',0
  _menu2 TCHAR 'Hide',0
  _menu3 TCHAR 'Autodisable',0
  _menu4 TCHAR 'Keep quiet',0
  _menu5 TCHAR 'Disable Now!',0
  _menu6 TCHAR 'Exit',0
  ;Mutex
  _mutex      TCHAR 'DA88ff88fadd17',0



  ;
  ; Variables
  ;
  align 4
  ; Handles:
  hWnd		dd ?
  hInstance	dd ?
  hIcon 	dd ?
  hMutex	dd ?
  ; Other:
  bNotify	dd ?	;boolean, TRUE when icon is in tray
  bytes_count	dd ?
  lastLCC	dd ?	;for storing last received LCC value

  align 4
  ;
  ; Structures
  ;
  msg		MSG
  nid		NOTIFYICONDATA2
  inbuffer	ATA_PASS_THROUGH_EX_WITH_BUFFERS		;operative buffer
  atabuffer	ATA_PASS_THROUGH_EX_WITH_BUFFERS		;information buffer
  gvp		GETVERSIONINPARAMS
  scip		SENDCMDINPARAMS
  scop		SENDCMDOUTPARAMS
  scopBuffer	db 512 dup (?)

  ;
  ; Arrays and buffers
  ;
  drivearray	db 64 dup (?)	; available drives array
  drivemodel	db 40 dup (?)
  szBuffer	TCHAR MAX_PATH dup (?)
  wszBuffer	TCHAR UNICODE_BUF_SIZE dup (?)

section '.rsrc' resource data readable

  IDT_ICON	= 100
  IDI_MAIN	= 101
  IDM_MENU	= 102

  IDC_DISABLENOW	= 150	;disable now button
  IDC_HIDE	= 151		;hide button
  IDC_EXIT	= 152		;exit button
  IDC_DRIVE	= 153		;drive selection drop-down list
  IDC_CURLCC	= 154	;current LCC edit-box
  IDC_AUTO	= 155	;auto check box
  IDC_ABOUT	= 156	;about button
  IDC_NONOTIF	= 157	;switch off notifications checkbox

  IDM_MENU_RESTORE	=160
  IDM_MENU_AUTO 	=161
  IDM_MENU_NOTIF	= 162

  WM_MYMESSAGE	= WM_USER+1
  IDD_MAIN	= 37

  IDT_TIMER	= 170

  directory RT_DIALOG,dialogs,\
	    RT_GROUP_ICON,group_icons,\
	    RT_ICON,icons,\
	    RT_MANIFEST,manifest;,\
 ;           RT_MENU,menus


  resource dialogs,\
	IDD_MAIN,LANG_NEUTRAL,disapm

  resource group_icons,\
	IDI_MAIN,LANG_NEUTRAL,main_icon
  
;  resource menus,\
;        IDM_MENU,LANG_NEUTRAL,context_menu

  resource icons,\
	16,LANG_NEUTRAL,icon16_data,\
	24,LANG_NEUTRAL,icon24_data



  resource manifest,\
	1,LANG_NEUTRAL,man


  icon main_icon,icon16_data,'images\disicon.ico',icon24_data,'images\disicon24.ico'


;  menu context_menu
;    menuitem 'X',0,MFR_POPUP+MFR_END
;      menuitem 'Restore',IDM_MENU_RESTORE,0
;      menuitem 'Autodisable',IDM_MENU_AUTO,0
;      menuseparator
;      menuitem 'Exit',IDC_EXIT,MFR_END

	
	

  dialog disapm,'Disable HDD APM',1,1,150,84,WS_VISIBLE + WS_CAPTION + DS_CENTER + WS_SYSMENU+DS_SETFONT,0,0,'MS Shell Dlg',8
    dialogitem 'BUTTON','Disable APM Now!',IDC_DISABLENOW,75,36,78,15,WS_CHILDWINDOW+WS_VISIBLE+WS_TABSTOP+BS_DEFPUSHBUTTON
    dialogitem 'BUTTON','Automatic mode',IDC_AUTO,3,54,65,9,WS_CHILDWINDOW+WS_VISIBLE+WS_TABSTOP+BS_AUTOCHECKBOX
    dialogitem 'BUTTON','Disable notifications',IDC_NONOTIF,75,54,90,9,WS_CHILDWINDOW+WS_VISIBLE+WS_TABSTOP+BS_AUTOCHECKBOX
    dialogitem 'BUTTON','Hide',IDC_HIDE,3,66,72,15,WS_CHILDWINDOW+WS_VISIBLE+WS_TABSTOP+BS_PUSHBUTTON
    dialogitem 'BUTTON','Exit',IDC_EXIT,75,66,72,15,WS_CHILDWINDOW+WS_VISIBLE+WS_TABSTOP+BS_PUSHBUTTON
    dialogitem 'STATIC','Drive',-1,3,3,54,9,WS_CHILDWINDOW+WS_VISIBLE
    dialogitem 'COMBOBOX','',IDC_DRIVE,3,12,144,39,WS_CHILDWINDOW+WS_VISIBLE+WS_TABSTOP+CBS_DROPDOWNLIST
    dialogitem 'STATIC','Load-Cycle Count value',-1,3,27,87,9,WS_CHILDWINDOW+WS_VISIBLE
    dialogitem 'EDIT','',IDC_CURLCC,3,36,71,15,WS_CHILD+WS_VISIBLE+ES_READONLY,WS_EX_CLIENTEDGE
  enddialog

  resdata man
    file 'dipasm.manifest.xml'
  endres


section '.idata' import data readable writeable

  library kernel32,'KERNEL32.DLL',\
	  user32,'USER32.DLL',\
	  shell32,'SHELL32.DLL'

  include 'api\kernel32.inc'
  include 'api\user32.inc'
  include 'api\shell32.inc'
