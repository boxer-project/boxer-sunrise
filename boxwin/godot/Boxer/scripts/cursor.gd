extends Control

@onready var transparent_stylebox = ResourceLoader.load("res://themes/transparent_stylebox.tres")
@onready var cursor_stylebox = ResourceLoader.load("res://themes/cursor_stylebox.tres")

var cursorOn = false
var cursorPaused = false

var cur_row
var cur_cha
var cur_idx = 0

func _process(delta: float) -> void:
    var name_edits = get_tree().get_nodes_in_group("NameInputs")
    var paused = false
    for name_edit in name_edits:
        if name_edit.has_focus():
            paused = true
    cursorPaused = paused
    
    var list = []
    list.size
    var cha: Control
    
    #print("Cursor process: ", cur_idx, " size: ", cur_row.chas.size())
    if cur_row and (cur_idx <= cur_row.chas.size()):
        if cur_idx == 0:
            self.global_position.x = cur_row.global_position.x
            self.global_position.y = cur_row.global_position.y
        elif cur_row.chas.size() > 0:
            cha = cur_row.chas[cur_idx-1]
            self.global_position.x = cha.global_position.x + cha.size.x
            self.global_position.y = cha.global_position.y
        else:
            self.global_position.x = cur_row.global_position.x
            self.global_position.y = cur_row.global_position.y

# Called when the node enters the scene tree for the first time.
func _ready() -> void:
    pass # Replace with function body.

func _on_blink_timer_timeout() -> void:
    if cursorOn == true and not cursorPaused:
        self.add_theme_stylebox_override("panel", cursor_stylebox)
        cursorOn = false
    else:
        self.add_theme_stylebox_override("panel", transparent_stylebox)
        cursorOn = true
        
func _box_background_color_changed(color):
    var box: BoxContainer
    box = cur_row.parent_box
    print("changing box background color", color, box)
    box.get_node("%OuterBorderPanel").add_theme_color_override("bg_color", color)
    box.get_node("%BoxPanel").add_theme_color_override("bg_color", color)
    #box.add_theme_color_override("bg_color", color)
        
