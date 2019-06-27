#include "HsFFI.h"
#import <Cocoa/Cocoa.h>
#import <WebKit/WebKit.h>

extern void callIO(HsStablePtr);
extern void callWithCString(const char * _Nonnull, HsStablePtr);

// Doesn't work - I think because we are redirecting stderr into NSLog, and
// NSLog redirects stuff back to stderr
//void writeLog (const char * _Nonnull log) {
//  NSLog(@"%@", [NSString stringWithCString:log encoding:NSUTF8StringEncoding]);
//}

void setupAppMenu() {
  NSApplication *application = [NSApplication sharedApplication];

  NSMenu *mainMenu = [[NSMenu alloc] init];
  NSMenuItem *appMenuItem = [mainMenu
    addItemWithTitle:@"Application"
    action: nil
    keyEquivalent:@""
  ];
  NSMenuItem *fileMenuItem = [mainMenu
    addItemWithTitle:@"File"
    action: nil
    keyEquivalent:@""
  ];
  NSMenuItem *editMenuItem = [mainMenu
    addItemWithTitle:@"Edit"
    action: nil
    keyEquivalent:@""
  ];
  NSMenuItem *viewMenuItem = [mainMenu
    addItemWithTitle:@"View"
    action: nil
    keyEquivalent:@""
  ];
  NSMenuItem *windowMenuItem = [mainMenu
    addItemWithTitle:@"Window"
    action: nil
    keyEquivalent:@""
  ];

  // App menu
  NSMenu *appMenu = [[NSMenu alloc] init];
  [mainMenu setSubmenu:appMenu forItem:appMenuItem];
  // About menu
  NSMenuItem *about = [appMenu
    addItemWithTitle:@"About Pact"
    action: @selector(orderFrontStandardAboutPanel:)
    keyEquivalent:@""
  ];
  [about setTarget:application];
  // Hide/show functionality
  [appMenu addItem:[NSMenuItem separatorItem]];
  NSMenuItem *hide = [appMenu addItemWithTitle:@"Hide Pact" action: @selector(hide:) keyEquivalent:@"h" ];
  [hide setTarget:application];
  NSMenuItem *hideOthers = [appMenu
    addItemWithTitle:@"Hide Others"
    action: @selector(hideOtherApplications:)
    keyEquivalent:@"h"
  ];
  [hideOthers setKeyEquivalentModifierMask:NSAlternateKeyMask|NSCommandKeyMask];
  [hideOthers setTarget:application];
  NSMenuItem *showAll = [appMenu addItemWithTitle:@"Show All" action: @selector(unhideAllApplications:) keyEquivalent:@"" ];
  [showAll setTarget:application];
  // Quit app
  [appMenu addItem:[NSMenuItem separatorItem]];
  NSMenuItem *quit = [appMenu
    addItemWithTitle:@"Quit Pact"
    action: @selector(terminate:)
    keyEquivalent:@"q"
  ];
  [quit setTarget:application];

  // File menu
  NSMenu *fileMenu = [[NSMenu alloc] initWithTitle:@"File"];
  [mainMenu setSubmenu:fileMenu forItem:fileMenuItem];
  NSMenuItem *new = [fileMenu
    addItemWithTitle:@"New"
    action: nil
    keyEquivalent:@"n"
  ];

  WKWebView *webView = [[application mainWindow] contentView];

  // Edit menu
  NSMenu *editMenu = [[NSMenu alloc] initWithTitle:@"Edit"];
  [mainMenu setSubmenu:editMenu forItem:editMenuItem];
  NSMenuItem *cut = [editMenu
    addItemWithTitle:@"Cut"
    action: @selector(cut:)
    keyEquivalent:@"x"
  ];
  [cut setTarget:webView];
  NSMenuItem *copy = [editMenu
    addItemWithTitle:@"Copy"
    action: @selector(copy:)
    keyEquivalent:@"c"
  ];
  [copy setTarget:webView];
  NSMenuItem *paste = [editMenu
    addItemWithTitle:@"Paste"
    action: @selector(paste:)
    keyEquivalent:@"v"
  ];
  [paste setTarget:webView];
  NSMenuItem *delete = [editMenu
    addItemWithTitle:@"Delete"
    action: @selector(delete:)
    keyEquivalent:@""
  ];
  [delete setTarget:webView];
  NSMenuItem *selectAll = [editMenu
    addItemWithTitle:@"Select All"
    action: @selector(selectAll:)
    keyEquivalent:@"a"
  ];
  [selectAll setTarget:webView];

  // View menu
  NSMenu *viewMenu = [[NSMenu alloc] initWithTitle:@"View"];
  [mainMenu setSubmenu:viewMenu forItem:viewMenuItem];
  NSMenuItem *fullscreen = [viewMenu
    addItemWithTitle:@"Toggle Full Screen"
    action: @selector(toggleFullScreen:)
    keyEquivalent:[NSString stringWithFormat:@"%c", NSCarriageReturnCharacter]
  ];
  [fullscreen setTarget:webView];

  // Window menu
  NSMenu *windowMenu = [[NSMenu alloc] initWithTitle:@"Window"];
  [mainMenu setSubmenu:windowMenu forItem:windowMenuItem];
  NSMenuItem *minimize = [windowMenu
    addItemWithTitle:@"Minimize"
    action: @selector(miniaturizeAll:)
    keyEquivalent:@"m"
  ];
  [minimize setTarget:webView];


  [application setMainMenu:mainMenu];

}
