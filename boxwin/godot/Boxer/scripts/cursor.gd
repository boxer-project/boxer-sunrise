extends Control

@onready var transparent_stylebox = ResourceLoader.load("res://themes/transparent_stylebox.tres")
@onready var cursor_stylebox = ResourceLoader.load("res://themes/cursor_stylebox.tres")

var cursorOn = false

var cur_row
var cur_cha
var cur_idx = 0

func _process(_delta: float) -> void:
    # This is happening in the process loop, because it might take a while for a cha's position to stabilize.
    # See skip_position member on chas, boxes
    update_location()

# Called when the node enters the scene tree for the first time.
func _ready() -> void:
    pass # Replace with function body.

func update_location():
    # Update the cursor location based on our cur_row and cur_idx
    if cur_row and (cur_idx <= cur_row.get_child_count()):
        if cur_idx == 0:
            self.global_position.x = cur_row.global_position.x
            self.global_position.y = cur_row.global_position.y
            self.size.y = 20
        elif cur_row.get_child_count() > 0:
            var cha = cur_row.get_child(cur_idx-1)
            if cha.skip_position:
                cha.skip_position = false
                return
            var x = cha.global_position.x + cha.size.x
            var y = cha.global_position.y
            self.global_position = Vector2(x, y)
            self.size.y = cha.size.y
        else:
            self.global_position.x = cur_row.global_position.x
            self.global_position.y = cur_row.global_position.y
            self.size.y = 20

func _on_blink_timer_timeout() -> void:
    if cursorOn == true:
        self.add_theme_stylebox_override("panel", cursor_stylebox)
        cursorOn = false
    else:
        self.add_theme_stylebox_override("panel", transparent_stylebox)
        cursorOn = true
