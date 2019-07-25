#include "HsFFI.h"
#import <Cocoa/Cocoa.h>
#import <WebKit/WebKit.h>

extern void callIO(HsStablePtr);
extern void callWithCString(const char * _Nonnull, HsStablePtr);

// This class is here so we can send messages to it from selectors
@interface DialogController:NSObject {
  NSOpenPanel *openFilePanel;
  HsStablePtr openFileHandler;
}
-(DialogController *) initWithHandler:(HsStablePtr)handler;
-(void) openFileDialog;
@end

DialogController *global_dialogController = 0;

// For use from haskell land
void global_openFileDialog() {
  // Add the operation to the queue to ensure it happens on main thread
  [[NSOperationQueue mainQueue] addOperationWithBlock:^(void) {
    [global_dialogController openFileDialog];
  }];
}

@implementation DialogController
-(id)initWithHandler:(HsStablePtr)handler {
  self = [super init];
  openFilePanel = [[NSOpenPanel openPanel] retain];
  openFileHandler = handler;
  [openFilePanel setCanChooseFiles:YES];
  [openFilePanel setAllowsMultipleSelection:NO];
  [openFilePanel setCanChooseDirectories:NO];
  NSArray *fileTypes = @[@"pact"];
  [openFilePanel setAllowedFileTypes:fileTypes];
  return self;
}
-(void) openFileDialog {
  if ([openFilePanel runModal] == NSFileHandlingPanelOKButton) {
    NSURL *url = [[openFilePanel URLs] objectAtIndex:0];
    callWithCString([url fileSystemRepresentation], openFileHandler);
  }
}
@end

// Doesn't work - I think because we are redirecting stderr into NSLog, and
// NSLog redirects stuff back to stderr
//void writeLog (const char * _Nonnull log) {
//  NSLog(@"%@", [NSString stringWithCString:log encoding:NSUTF8StringEncoding]);
//}

void activateWindow() {
  [[NSOperationQueue mainQueue] addOperationWithBlock:^(void) {
    NSApplication *app = [NSApplication sharedApplication];
    NSWindow *window = [app mainWindow];
    [[window contentView] setHidden: NO];
  }];
}

void hideWindow() {
  [[NSOperationQueue mainQueue] addOperationWithBlock:^(void) {
    NSApplication *app = [NSApplication sharedApplication];
    NSWindow *window = [app mainWindow];
    [[window contentView] setHidden: YES];
  }];
}

void setupAppMenu(HsStablePtr hs_handleOpenedFile) {
  NSApplication *application = [NSApplication sharedApplication];
  global_dialogController = [[DialogController alloc] initWithHandler:hs_handleOpenedFile];

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

  NSMenuItem *open = [fileMenu
    addItemWithTitle:@"Open"
    action: @selector(openFileDialog)
    keyEquivalent:@"o"
  ];
  [open setTarget:global_dialogController];

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
