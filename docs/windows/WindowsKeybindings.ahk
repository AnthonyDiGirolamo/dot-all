#NoEnv  ; Recommended for performance and compatibility with future AutoHotkey releases.
; #Warn  ; Enable warnings to assist with detecting common errors.
SendMode Input  ; Recommended for new scripts due to its superior speed and reliability.
SetWorkingDir %A_ScriptDir%  ; Ensures a consistent starting directory.

Capslock::Ctrl

AppsKey::RWin

;$~*Ctrl::
;if !state
;       state :=  (GetKeyState("Shift", "P") ||  GetKeyState("Alt", "P") || GetKeyState("LWin", "P") || GetKeyState("RWin", "P"))
;return

;$~ctrl up::
;if instr(A_PriorKey, "control") && !state
;       send {esc}
;state := 0
;return

;space::
;Send {space}
;return

;space & 1:: Send !
;space & 2:: Send @
;space & 3:: Send #
;space & 4:: Send $
;space & 5:: Send {`%}
;space & 6:: Send ^
;space & 7:: Send &
;space & 8:: Send *
;space & 9:: Send (
;space & 0:: Send )
;space & -:: Send _
;space & =:: Send +
;space & q:: Send Q
;space & w:: Send W
;space & e:: Send E
;space & r:: Send R
;space & t:: Send T
;space & y:: Send Y
;space & u:: Send U
;space & i:: Send I
;space & o:: Send O
;space & p:: Send P
;space & [:: Send {`{}
;space & ]:: Send {`}}
;space & a:: Send A
;space & s:: Send S
;space & d:: Send D
;space & f:: Send F
;space & g:: Send G
;space & h:: Send H
;space & j:: Send J
;space & k:: Send K
;space & l:: Send L
;space & `;:: Send :
;space & ':: Send "
;space & z:: Send Z
;space & x:: Send X
;space & c:: Send C
;space & v:: Send V
;space & b:: Send B
;space & n:: Send N
;space & m:: Send M
;space & ,:: Send <
;space & .:: Send >
;space & /:: Send ?
;space & \:: Send |
;return

;space & h:: Send {Left}
;space & n:: Send {Down}
;space & e:: Send {Up}
;space & l:: Send {Right}
;return
