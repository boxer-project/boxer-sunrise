extends Control

var centers = [
    Vector2(0,0),
    Vector2(50,50),
    Vector2(0,100),
    Vector2(100,100)
]

func _draw() -> void:
    for c in centers:
        draw_circle(c, 40, Color.BLACK)

    # var c = centers.pop_front()
    # if c:
    #     draw_circle(c, 40, Color.BLACK)
