extends Label

var color setget _set_color
var value setget _set_text

func _set_color(col):
#	color_rect.color = col	
	color = col
	
func _set_text(val):
	text = val	
	value = val
