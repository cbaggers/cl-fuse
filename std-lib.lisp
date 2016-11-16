(in-package :fuse.controls)

(defparameter std-lib-name-map
  '((activated "Activated")
    (activating-animation "ActivatingAnimation")
    (adding-animation "AddingAnimation")
    (alternate-root "AlternateRoot")
    (android "Android")
    (back-button "BackButton")
    (blur "Blur")
    (bottom-bar-background "BottomBarBackground")
    (bottom-frame-background "BottomFrameBackground")
    (button "Button")
    (button-base "ButtonBase")
    (circle "Circle")
    (circular-range-behavior "CircularRangeBehavior")
    (clicked "Clicked")
    (client-panel "ClientPanel")
    (closure "Closure")
    (cone-renderer "ConeRenderer")
    (container "Container")
    (containing-text "ContainingText")
    (content-control "ContentControl")
    (cube-renderer "CubeRenderer")
    (cylinder-renderer "CylinderRenderer")
    (deactivated "Deactivated")
    (deactivating-animation "DeactivatingAnimation")
    (default-scroller "DefaultScroller")
    (default-trigger "DefaultTrigger")
    (deferred "Deferred")
    (desaturate "Desaturate")
    (deselected "Deselected")
    (direct-navigation "DirectNavigation")
    (directional-light "DirectionalLight")
    (dock-panel "DockPanel")
    (double-clicked "DoubleClicked")
    (double-tapped "DoubleTapped")
    (draggable "Draggable")
    (drop-shadow "DropShadow")
    (each "Each")
    (edge-navigation "EdgeNavigation")
    (edge-navigator "EdgeNavigator")
    (edge-swipe-animation "EdgeSwipeAnimation")
    (ellipse "Ellipse")
    (enter-horizontal "EnterHorizontal")
    (enter-vertical "EnterVertical")
    (entered-force-field "EnteredForceField")
    (entering-animation "EnteringAnimation")
    (entity "Entity")
    (environment-light "EnvironmentLight")
    (exit-horizontal "ExitHorizontal")
    (exit-vertical "ExitVertical")
    (exited-force-field "ExitedForceField")
    (exiting-animation "ExitingAnimation")
    (file-image-source "FileImageSource")
    (find-edges "FindEdges")
    (frustum "Frustum")
    (graphics-view "GraphicsView")
    (grid "Grid")
    (halftone "Halftone")
    (hierarchical-navigation "HierarchicalNavigation")
    (http-image-source "HttpImageSource")
    (image "Image")
    (in-force-field-animation "InForceFieldAnimation")
    (instance "Instance")
    (instantiator "Instantiator")
    (interaction-completed "InteractionCompleted")
    (interactive-transform "InteractiveTransform")
    (ios "iOS")
    (java-script "JavaScript")
    (keep-focus-in-view "KeepFocusInView")
    (keep-in-view "KeepInView")
    (keep-in-view-common "KeepInViewCommon")
    (layout-animation "LayoutAnimation")
    (layout-control "LayoutControl")
    (linear-navigation "LinearNavigation")
    (linear-range-behavior "LinearRangeBehavior")
    (long-pressed "LongPressed")
    (map-marker "MapMarker")
    (map-view "MapView")
    (mask "Mask")
    (match "Match")
    (mesh-renderer "MeshRenderer")
    (multi-density-image-source "MultiDensityImageSource")
    (multi-layout "MultiLayout")
    (multi-layout-panel "MultiLayoutPanel")
    (native-view-host "NativeViewHost")
    (nav-enter-horizontal "NavEnterHorizontal")
    (nav-exit-horizontal "NavExitHorizontal")
    (nav-remove-horizontal "NavRemoveHorizontal")
    (navigation-bar "NavigationBar")
    (navigator "Navigator")
    ;;(number "Number")
    (on-back-button "OnBackButton")
    (on-key-press "OnKeyPress")
    (on-user-event "OnUserEvent")
    (page "Page")
    (page-begin-loading "PageBeginLoading")
    (page-control "PageControl")
    (page-indicator "PageIndicator")
    (page-indicator-dot "PageIndicatorDot")
    (page-loaded "PageLoaded")
    (page-view "PageView")
    (pan-gesture "PanGesture")
    (panel "Panel")
    (path "Path")
    (path-shape "PathShape")
    (placeholder "Placeholder")
    (point-attractor "PointAttractor")
    (point-light "PointLight")
    (pressed "Pressed")
    (progress-animation "ProgressAnimation")
    (pull-to-reload "PullToReload")
    (range-animation "RangeAnimation")
    (range-control "RangeControl")
    (range-control-2d "RangeControl2D")
    (rectangle "Rectangle")
    (regular-polygon "RegularPolygon")
    (released "Released")
    (removing-animation "RemovingAnimation")
    (render-node "RenderNode")
    (render-to-texture "RenderToTexture")
    (resource-bool "ResourceBool")
    (resource-float "ResourceFloat")
    (resource-float-2 "ResourceFloat2")
    (resource-float-3 "ResourceFloat3")
    (resource-float-4 "ResourceFloat4")
    (resource-object "ResourceObject")
    (resource-string "ResourceString")
    (resources "Resources")
    (right-frame-background "RightFrameBackground")
    (root-viewport "RootViewport")
    (rotate-gesture "RotateGesture")
    (rotation "Rotation")
    (router "Router")
    (scaling "Scaling")
    (scene "Scene")
    (scroll-view "ScrollView")
    (scroll-view-base "ScrollViewBase")
    (scrolled "Scrolled")
    (scroller "Scroller")
    (scrolling-animation "ScrollingAnimation")
    (select "Select")
    (selectable "Selectable")
    (selected "Selected")
    (selection "Selection")
    ;;(shadow "Shadow")
    (shear "Shear")
    (slider "Slider")
    (sphere-renderer "SphereRenderer")
    (spot-light "SpotLight")
    (spring "Spring")
    (stack-panel "StackPanel")
    (star "Star")
    (state "State")
    (state-group "StateGroup")
    (status-bar-background "StatusBarBackground")
    (status-bar-config "StatusBarConfig")
    (swipe-gesture "SwipeGesture")
    (swipe-navigate "SwipeNavigate")
    (swiped "Swiped")
    (swiping-animation "SwipingAnimation")
    (switch "Switch")
    (tapped "Tapped")
    (text "Text")
    (text-block "TextBlock")
    (text-box "TextBox")
    (text-input "TextInput")
    (text-input-action-triggered "TextInputActionTriggered")
    (text-view "TextView")
    (texture-image-source "TextureImageSource")
    (timeline "Timeline")
    (toggle-control "ToggleControl")
    (top-frame-background "TopFrameBackground")
    (trackball "Trackball")
    (transform-3d "Transform3D")
    (transition "Transition")
    (translation "Translation")
    (user-event "UserEvent")
    (video "Video")
    (viewbox "Viewbox")
    (viewport "Viewport")
    (web-view "WebView")
    (while-active "WhileActive")
    (while-busy "WhileBusy")
    (while-can-go-back "WhileCanGoBack")
    (while-can-go-forward "WhileCanGoForward")
    (while-completed "WhileCompleted")
    (while-contains-text "WhileContainsText")
    (while-count "WhileCount")
    (while-disabled "WhileDisabled")
    (while-dragging "WhileDragging")
    (while-empty "WhileEmpty")
    (while-enabled "WhileEnabled")
    (while-failed "WhileFailed")
    (while-false "WhileFalse")
    (while-float "WhileFloat")
    (while-focus-within "WhileFocusWithin")
    (while-focused "WhileFocused")
    (while-hovering "WhileHovering")
    (while-in-enter-state "WhileInEnterState")
    (while-in-exit-state "WhileInExitState")
    (while-inactive "WhileInactive")
    (while-interacting "WhileInteracting")
    (while-keyboard-visible "WhileKeyboardVisible")
    (while-loading "WhileLoading")
    (while-navigating "WhileNavigating")
    (while-not-empty "WhileNotEmpty")
    (while-not-focused "WhileNotFocused")
    (while-page-loading "WhilePageLoading")
    (while-paused "WhilePaused")
    (while-playing "WhilePlaying")
    (while-pressed "WhilePressed")
    (while-scrollable "WhileScrollable")
    (while-scrolled "WhileScrolled")
    (while-selected "WhileSelected")
    (while-string "WhileString")
    (while-swipe-active "WhileSwipeActive")
    (while-true "WhileTrue")
    (while-visible "WhileVisible")
    (while-visible-in-scroll-view "WhileVisibleInScrollView")
    (while-window-landscape "WhileWindowLandscape")
    (while-window-portrait "WhileWindowPortrait")
    (while-window-size "WhileWindowSize")
    (with "With")
    (wrap-panel "WrapPanel")
    (zoom-gesture "ZoomGesture")))

(defmacro dumb-macro-shiz ()
  `(progn
     ,@(loop :for (s n) :in std-lib-name-map :collect
          `(defmacro ,s (args &body body)
             (append (list ,n args) body)))))

(dumb-macro-shiz)


(parenscript:defpsmacro observable (&rest args)
  `(#:|Observable| ,@args))
