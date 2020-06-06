#NoEnv  ; Recommended for performance and compatibility with future AutoHotkey releases.
; #Warn  ; Enable warnings to assist with detecting common errors.
SendMode Input  ; Recommended for new scripts due to its superior speed and reliability.
SetWorkingDir %A_ScriptDir%  ; Ensures a consistent starting directory.

SetCapsLockState Off

CapsLock::
	key=
	Input, key, B C L1 T1, {Esc}
	if (ErrorLevel = "Max")
		Send {Ctrl Down}%key%
	KeyWait, CapsLock
	Return
CapsLock up::
	If key
		Send {Ctrl Up}
	else
		if (A_TimeSincePriorHotkey < 1000)
			Send, {Esc 2}
	Return
