extends Control

#
# Demo: Working prototype of resizing a Box control node using the lower right
#       corner.  Likely there are a number of things we could clean up and make
#       more accurate, precise, performant, but this is a simple working example
#       we can build off of in the future.
#      
#       Steven Githens 2025-06-12
#
var moving = false
var moved = false

func _on_lower_right_corner_gui_input(event: InputEvent) -> void:
    if event is InputEventMouseButton and event.pressed:
        print("Button down event: ", event)
        moving = event.pressed
    elif event is InputEventMouseButton and not event.pressed:
        print("Button up event2: ", event)
        moving = event.pressed
        if not moved:
            # single click means to set the box back to fixed size
            self.size.x = 848
            self.size.y = 460
        moved = false        
    elif event is InputEventMouseMotion and moving:
        print("Relative change: ", event.relative.x, " , ", event.relative.y)
        moved = true
        self.size.x = self.size.x + event.relative.x
        self.size.y = self.size.y + event.relative.y
