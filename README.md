## FAQ ##

  **Q:** What is sunwm? 

  **A:** sunwm is a manual tiling window manager for X11, written in Haskell.
     Many of its features are inspired by Ratpoison/StumpWM and the text editor Vim.

  -- 

  **Q:** How do I configure sunwm?
  
  **A:** sunwm is configured solely through the file sunwm.hs, then recompiled from source.
     Since sunwm.hs is a haskell source file, one may use the Haskell language itself
     to configure sunwm.  Typically this is not necessary and simple modification of values
     in the default sunwm.hs is all that is needed.  This requires no knowledge of Haskell.

  --

  **Q:** How do I use sunwm?

  **A:** The vast majority of interaction between sunwm and the user is done through the keyboard.
     There are two main categories of keyboard shortcuts: prefix commands and top-level commands.
     For prefix commands, first press the prefix key designated in your configuration (default: Ctrl+t).
     The mouse cursor shape will change, indicating you are in prefix mode.  At this point you may
     press any of the key combinations designated in your sunwm.hs to perform wm actions.  The command
     will be executed and you will be taken out of prefix mode.  When you are out of prefix mode, you may
     still interact with the window manager through top-level keybinds as defined in sunwm.hs.

## CURRENT FEATURES ##

 * Manual tyling in the style of Ratpoison/StumpWM (using prefix key).
 * Integrated support for floating windows (in the style of dwm).
 * Supports multiple workspaces with independent layouts.
 * dwm-style status bar provided by XMobar.
 * Written in Haskell and based on a core of heavily-tested purely-functional data structures/operations.
 * Configured via a Haskell source file for added customizeability.

## PLANNED FEATURES ##

 * Add multiple methods of focusing windows with mouse (click-to-focus, follow-mouse, disabled)
 * Add support for multiple displays.
 * Add support for user-defined hooks (ex: startup, split, change desktop, etc)
