extends Node

@export var cha_scene: PackedScene
@export var box_scene: PackedScene
@export var row_scene: PackedScene
@export var turtle_scene: PackedScene

@onready var open_dialog: FileDialog = get_node("/root/Main/OpenFileDialog")

# Whether to use the Boxer GDExtension or the prototype mode with Godot handling the input
@export var use_boxer_gdextension = false

# Keep track of our cursor which we move around the node tree
var cursor
var outermost_box = null

var canvas_zoom = 1:
    get:
        return canvas_zoom
    set(value):
        # TODO max and min
        canvas_zoom = value

@export var show_toolbar = true:
    get:
        return show_toolbar
    set(value):
        show_toolbar = value
        if %ToolbarContainer:
            %ToolbarContainer.visible = value

@export var show_statusbar = true:
    get:
        return show_statusbar
    set(value):
        show_statusbar = value
        if %StatusBarContainer:
            %StatusBarContainer.visible = value

var lisp_thread: Thread

# Called when the node enters the scene tree for the first time.
func _ready() -> void:
    boxer_event_queue_mutex = Mutex.new()
    boxer_scene_queue_mutex = Mutex.new()
    cursor = $Cursor
    # Have the main world box take up the entire screen
    %World/BoxInternals.custom_minimum_size = ((get_viewport().size - Vector2i(20, 20)) / Global.screen_scale)
    outermost_box = %World
    get_viewport().size_changed.connect(_root_viewport_size_changed)
    _root_viewport_size_changed()

    if use_boxer_gdextension:
        print("About to bootstrap THREADED *initial-box*")

    lisp_thread = Thread.new()
    var world_node = get_node("/root/Main/TopLevelContainer/OutermostBoxScroll/World")
    var first_row_node = get_node("/root/Main/TopLevelContainer/OutermostBoxScroll/World/BoxInternals/OuterBorderPanel/BoxPanel/PanelContainer/RowsBox/Row")
    first_row_node.parent_box = world_node
    lisp_thread.start(_start_lisp.bind($GDBoxer, get_node("/root/Main"), world_node, first_row_node))

    _on_gd_boxer_boxer_point_location(first_row_node, 0)

func _notification(what):
    if what == NOTIFICATION_WM_CLOSE_REQUEST:
        boxer_event_queue_mutex.lock()
        boxer_event_queue.push_front([4])
        boxer_event_queue_mutex.unlock()
        lisp_thread.wait_to_finish()
        get_tree().quit() # default behavior

###
### Access to embedded boxer
###
func _start_lisp(lisp_node, main_node, world_node, first_row_node):
    lisp_node.startup_lisp(main_node, world_node, first_row_node)

# boxer_event_queue will be
# 0 - Type of event
#     0 - nothing (this is not actually pushed from here ever)
#     1 - Keyboard event
#     2 - Mouse event
#     3 - Lisp Function call in Boxer package
#         ex. [3, 2, "functionname", "arg1", "etc"]
#     4 - Shutdown and exit
var boxer_event_queue = []
var boxer_event_queue_mutex: Mutex
func fetch_event_from_queue():
    boxer_event_queue_mutex.lock()
    var next = boxer_event_queue.pop_back()
    boxer_event_queue_mutex.unlock()
    if next:
        return next
    else:
        return []

func handle_character_input(code, bits):
    boxer_event_queue_mutex.lock()
    boxer_event_queue.push_front([1, code, bits])
    boxer_event_queue_mutex.unlock()

func handle_boxer_func_0(func_name):
    if boxer_event_queue_mutex:
        boxer_event_queue_mutex.lock()
        boxer_event_queue.push_front([3, 0, func_name])
        boxer_event_queue_mutex.unlock()

func handle_boxer_func_1(func_name, arg0):
    if boxer_event_queue_mutex:
        boxer_event_queue_mutex.lock()
        boxer_event_queue.push_front([3, 1, func_name, arg0])
        boxer_event_queue_mutex.unlock()

func handle_open_file(path):
    handle_boxer_func_1("GODOT-OPEN-FILE", path)

func handle_mouse_input(action, row, pos, click, bits, area):
    boxer_event_queue_mutex.lock()
    boxer_event_queue.push_front([2, action, row, pos, click, bits, area])
    boxer_event_queue_mutex.unlock()

func handle_paste_text(text):
    handle_boxer_func_1("GODOT-PASTE-TEXT", text)

func handle_request_cursor_update():
    boxer_event_queue_mutex.lock()
    boxer_event_queue.push_front([3, 0, "GODOT-UPDATE-POINT-LOCATION"])
    boxer_event_queue_mutex.unlock()

func handle_toggle_closet():
    boxer_event_queue_mutex.lock()
    boxer_event_queue.push_front([3, 0, "COM-TOGGLE-CLOSETS"])
    boxer_event_queue_mutex.unlock()


###
### Queue from Lisp -> Boxer
###
var boxer_scene_queue = []
var boxer_scene_queue_mutex: Mutex

# The scene queue is an array or arrays. The elements of each array are:
#   0 - godot node to apply function call to
#   1 - String with the name of the function
#   2..x Remaining arguments to function
func push_to_scene_queue(arr):
    boxer_scene_queue_mutex.lock()
    boxer_scene_queue.push_front(arr)
    boxer_scene_queue_mutex.unlock()

func handle_scene_queue():
    boxer_scene_queue_mutex.lock()
    var next = boxer_scene_queue.pop_back()
    while next:
        print("Applying: ", next[0], " : ", next[1], " : ", next.slice(2, next.size()))
        next[0].callv(next[1], next.slice(2, next.size()))
        next = boxer_scene_queue.pop_back()
    boxer_scene_queue_mutex.unlock()

###
### End access to embedded boxer
###



# Called when the root's viewport size changes (i.e. when the window is resized).
# This is done to handle multiple resolutions without losing quality.
func _root_viewport_size_changed() -> void:
    # The viewport is resized depending on the window height.
    # To compensate for the larger resolution, the viewport sprite is scaled down.
    %TopLevelContainer.size.x = Global.dpi_scale(get_viewport().size.x)
    %TopLevelContainer.size.y = Global.dpi_scale(get_viewport().size.y)
    %OutermostBoxScroll.size.y = Global.dpi_scale(get_viewport().size.y - %ToolbarContainer.size.y - %MessageBarContainer.size.y - %StatusBarContainer.size.y)


func _process(_delta: float) -> void:
    %World.scale = Vector2(canvas_zoom, canvas_zoom)
    %ZoomStatus.text = "Zoom {0}%".format([canvas_zoom * 100])
    if boxer_scene_queue.size() > 0:
        handle_scene_queue()

    if Input.is_action_just_pressed("ui_paste"):
        paste_to_boxer()
    if Input.is_action_just_pressed("ui_zoom_in"):
        canvas_zoom += 0.25
    if Input.is_action_just_pressed("ui_zoom_out"):
        canvas_zoom -= 0.25
    if Input.is_action_just_pressed("ui_open"):
        open_dialog.popup_centered()
    if Input.is_action_just_pressed("ui_save"):
        handle_boxer_func_0("COM-SAVE-DOCUMENT")
    if Input.is_action_just_pressed("ui_toggle_closet"):
        print("Boxer Toggle Closet")
        handle_toggle_closet()


func paste_to_boxer():
    if DisplayServer.clipboard_has():
        handle_paste_text(DisplayServer.clipboard_get())
    elif DisplayServer.clipboard_has_image():
        print("TODO Paste Image from Clipboard")

func _on_box_full_screened(box) -> void:
    print("Full Screening box: ", box)
    box.position = Vector2(0, 0)
    # TODO duplicated size calculation from _ready
    box.get_node("BoxInternals").custom_minimum_size = (get_viewport().size - Vector2i(20, 20)) / Global.screen_scale

func _on_box_flipped(box) -> void:
    pass
    ###
    ### Below is the prototype code for animated box flipping using a texture and viewport
    ###
    #var padding := 7
    #var box_outline := Rect2(box.global_position.x - padding, box.global_position.y, box.size.x + padding + 5, box.size.y + padding)
    #print("Signal flipped box", box_outline)
    #var img := get_viewport().get_texture().get_image().get_region(box_outline)
    #$ScreenClip.texture = ImageTexture.create_from_image(img)
    #var material = $FlippingViewport/FlippingBox.get_surface_override_material(0)
    #material.albedo_texture = ImageTexture.create_from_image(img)
    #$FlippingViewport/FlippingBox.flipping = true

func eclboxer_key_input(event: InputEventKey) -> void:
    # Sends keys/events to the ECL Backend
    var bits = 0
    # https://docs.godotengine.org/en/stable/classes/class_%40globalscope.html#enum-globalscope-key
    if event is InputEventKey and event.pressed:
        # TODO Ideally Shift should be bit 1 but there are some edge cases we need to review before
        #      adding that back in here.
        if event.shift_pressed:
            bits = bits | 1
        if event.ctrl_pressed:
            bits = bits | 2
        if event.alt_pressed:
            bits = bits | 4
        if event.meta_pressed:
            bits = bits | 8
        print("\n>>>>> eclboxer_key_input: ", event, " ||| ctrl: ", event.ctrl_pressed, " mask: ", event.get_modifiers_mask(),
          " bits: ", bits)
        if event.keycode == KEY_BACKSPACE:
            handle_character_input(8, bits)
        elif event.keycode == KEY_UP:
            handle_character_input(-1, bits)
        elif event.keycode == KEY_DOWN:
            handle_character_input(-2, bits)
        elif event.keycode == KEY_LEFT:
            handle_character_input(-3, bits)
        elif event.keycode == KEY_RIGHT:
            handle_character_input(-4, bits)
        elif event.keycode == KEY_ENTER:
            handle_character_input(13, bits)
        # TODO hack for testing turtles
        elif event.keycode == KEY_T and bits == 4:
            handle_character_input(116, bits)
        elif event.keycode < 4194304:
            #print("Handling InputEvent: ", event)
            # It seems that with a modifier, something like Ctrl-A returns 0 as the event.unicode. If this is
            # the case we'll use the keycode.  This entire arena of issues requires more looking in to.
            if event.unicode == 0:
                handle_character_input(event.keycode, bits)
            else:
                handle_character_input(event.unicode, bits)
        else:
            pass

func _unhandled_key_input(event: InputEvent) -> void:
    #print("Main scene input: ", event, " Ctrl: ", event.ctrl_pressed)
    if use_boxer_gdextension:
        eclboxer_key_input(event)

func _on_open_file_dialog_file_selected(path: String) -> void:
    print("Opening file: ", path)
    handle_open_file(path)
    $%OutermostBoxScroll.grab_focus()
    get_tree().call_group("NotCursorInput", "release_focus")

func _on_open_file_dialog_canceled() -> void:
    get_tree().call_group("NotCursorInput", "release_focus")

#
# SIGNALS FROM BOXER ENGINE
#
func _on_gd_boxer_boxer_insert_cha(row: Node, ch, cha_no: int) -> void:
    # print("_on_gd_boxer_boxer_insert_cha: ", row, " ", ch)
    if not row:
        print("Why is there no row to print: ", ch, " on???")
        return
    if typeof(ch) == TYPE_INT:
        var cha = cha_scene.instantiate()
        cha.text = String.chr(ch)
        row.add_cha(cha, cha_no)
    else:
        row.add_cha(ch, cha_no)

func _on_gd_boxer_boxer_delete_cha(row: Object, cha_no: int) -> void:
    row.remove_cha(cha_no)

func _on_gd_boxer_boxer_delete_chas_between_cha_nos(row: Object, strt_cha_no: int, stop_cha_no: int) -> void:
    row.remove_chas(strt_cha_no, stop_cha_no)

func _on_gd_boxer_boxer_point_location(row: Object, cha_no: int) -> void:
    if not row:
        print("Ugh, where is the row2??")
        # handle_request_cursor_update()
    else:
        cursor.cur_row = row
        cursor.cur_idx = cha_no

func make_box(boxer_box) -> BoxContainer:
    # Optional first_row to use
    var box = box_scene.instantiate()
    box.boxer_box = boxer_box
    box.full_screened.connect(_on_box_full_screened)
    box.flipped.connect(_on_box_flipped)
    box.current_row = null
    box.first_inferior_row = null
    box.delete_row_at_row_no(0)
    return box

func make_row(boxer_row): # -> HBoxContainer:
    var row = row_scene.instantiate()
    row.boxer_row = boxer_row
    return row

func make_turtle(boxer_turtle):
    var turtle = turtle_scene.instantiate()
    turtle.boxer_turtle = boxer_turtle
    turtle.append_draw_command([63, 0, 0, 10])
    return turtle
