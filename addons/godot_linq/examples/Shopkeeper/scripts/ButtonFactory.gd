class_name ButtonFactory
const ButtonInvItem = preload("../scenes/ButtonInventoryItem.tscn")

# TODO: take items list in constructor?

func _create_button_text(display_name, amt, cost):
	var text = "%s\nvalue:%d\namt %d" %\
		[display_name, cost, amt]
	return text

func _instantiate_button(text, tooltip=""):
	var btn = ButtonInvItem.instance()
	btn.get_node("Label").text = text
	btn.hint_tooltip = tooltip
	return btn
	
func create_button_inv_item(display_name, amt, cost, tooltip):
	var text = _create_button_text(display_name, amt, cost)
	var btn = _instantiate_button(text, tooltip)
	return btn