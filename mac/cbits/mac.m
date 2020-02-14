#include "HsFFI.h"
#import <Cocoa/Cocoa.h>
#import <WebKit/WebKit.h>

#ifndef MIN
#define MIN(a,b) ( ((a) < (b)) ? (a) : (b) )
#endif

extern void callIO(HsStablePtr);
extern void callWithCString(const char * _Nonnull, HsStablePtr);

// This class is here so we can send messages to it from selectors
@interface OpenDialogController:NSObject {
  NSOpenPanel *openFilePanel;
  HsStablePtr openFileHandler;
}
-(OpenDialogController *) initWithHandler:(HsStablePtr)handler;
-(void) openFileDialog:(NSString *)fileType;
-(void) openPactFileDialog;
@end

// This doesn't technically need to be a separate controller thing because
// we don't call it from the menu, but it also probably doesn't hurt. I have
// no idea whether we need to free the panels if we created one every call
// instead of it being a controller, so...
@interface SaveDialogController:NSObject {
  NSSavePanel *saveFilePanel;
  HsStablePtr dirSelectedHandler;
}
-(SaveDialogController *) init;
-(void) openSaveDialog:(HsStablePtr)handler;
@end

OpenDialogController *global_openDialogController = 0;
SaveDialogController *global_saveDialogController = 0;

// This function will return ~/ only when the app is not quarantined in a sandbox
const char *global_getHomeDirectory() {
  NSString *dir = NSHomeDirectory();
  return [dir UTF8String];
}

const char *global_getBundleIdentifier() {
  NSString *bundleIdentifier = [[NSBundle mainBundle] bundleIdentifier];
  return [bundleIdentifier UTF8String];
}

// For use from haskell land
void global_openFileDialog(char * cFileType) {
  NSString * fileType = [NSString stringWithCString:cFileType];
  // Add the operation to the queue to ensure it happens on main thread
  [[NSOperationQueue mainQueue] addOperationWithBlock:^(void) {
    [global_openDialogController openFileDialog:fileType];
  }];
}
void global_openSaveDialog(HsStablePtr handler) {
  [[NSOperationQueue mainQueue] addOperationWithBlock:^(void) {
    [global_saveDialogController openSaveDialog:handler];
  }];
}

@implementation OpenDialogController
-(id)initWithHandler:(HsStablePtr)handler {
  self = [super init];
  openFilePanel = [[NSOpenPanel openPanel] retain];
  openFileHandler = handler;
  [openFilePanel setCanChooseFiles:YES];
  [openFilePanel setAllowsMultipleSelection:NO];
  [openFilePanel setCanChooseDirectories:NO];

  return self;
}
-(void) openFileDialog:(NSString *)fileType {
  NSArray *fileTypes = @[fileType];
  [openFilePanel setAllowedFileTypes:fileTypes];

  if ([openFilePanel runModal] == NSFileHandlingPanelOKButton) {
    NSURL *url = [[openFilePanel URLs] objectAtIndex:0];
    callWithCString([url fileSystemRepresentation], openFileHandler);
  }
}
-(void) openPactFileDialog {
  [self openFileDialog:@"pact"];
}
@end

@implementation SaveDialogController
-(id)init {
  self = [super init];
  saveFilePanel = [[NSSavePanel alloc] init];
  [saveFilePanel setCanCreateDirectories:YES];

  return self;
}
-(void) openSaveDialog:(HsStablePtr)handler {
  if ([saveFilePanel runModal] == NSFileHandlingPanelOKButton) {
    NSURL *url = [saveFilePanel URL];
    callWithCString([url fileSystemRepresentation], handler);
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

void resizeWindow(int minWidth, int minHeight) {
  [[NSOperationQueue mainQueue] addOperationWithBlock:^(void) {
    NSApplication *application = [NSApplication sharedApplication];
    NSWindow *window = [application mainWindow];
    [window setContentSize: NSMakeSize(minWidth, minHeight)];
    [window center];
  }];
}

void moveToForeground() {
  [[NSOperationQueue mainQueue] addOperationWithBlock:^(void) {
    [NSApp activateIgnoringOtherApps:YES];
  }];
}

void moveToBackground() {
  [[NSOperationQueue mainQueue] addOperationWithBlock:^(void) {
    NSApplication *app = [NSApplication sharedApplication];
    NSWindow *window = [app mainWindow];
    [window orderBack:window];
  }];
}

void setupAppMenu(HsStablePtr hs_handleOpenedFile) {
  NSApplication *application = [NSApplication sharedApplication];
  global_openDialogController = [[OpenDialogController alloc] initWithHandler:hs_handleOpenedFile];
  global_saveDialogController = [[SaveDialogController alloc] init];

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
    addItemWithTitle:@"About Chainweaver"
    action: @selector(orderFrontStandardAboutPanel:)
    keyEquivalent:@""
  ];
  [about setTarget:application];
  // Hide/show functionality
  [appMenu addItem:[NSMenuItem separatorItem]];
  NSMenuItem *hide = [appMenu addItemWithTitle:@"Hide Chainweaver" action: @selector(hide:) keyEquivalent:@"h" ];
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
    addItemWithTitle:@"Quit Chainweaver"
    action: @selector(terminate:)
    keyEquivalent:@"q"
  ];
  [quit setTarget:application];

  // File menu
  NSMenu *fileMenu = [[NSMenu alloc] initWithTitle:@"File"];
  [mainMenu setSubmenu:fileMenu forItem:fileMenuItem];

  NSMenuItem *open = [fileMenu
    addItemWithTitle:@"Open"
    // TODO: This is broken now because it needs an arg
    action: @selector(openPactFileDialog)
    keyEquivalent:@"o"
  ];
  [open setTarget:global_openDialogController];

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
