extends Control


func _on_clear_button_pressed() -> void:
    print ("What the clear??")
    %Turtle.centers = [
        Vector2(90, 10)
    ]
    %Turtle.queue_redraw()

var canvas_img: Image

func _ready() -> void:
    canvas_img = %SubViewport.get_texture().get_image()

func _on_snap_button_pressed() -> void:
    var new_img = %SubViewport.get_texture().get_image()
    canvas_img.blend_rect(new_img, Rect2i(Vector2(0,0), canvas_img.get_size()), Vector2(0,0))

    var tex = ImageTexture.create_from_image(canvas_img)
    %Captured.texture = tex

