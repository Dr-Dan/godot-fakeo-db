extends VBoxContainer

var display_name setget _set_display_name
var cash setget _set_cash

func _set_display_name(val):
	$Name/Label.text = val
	display_name = val
	
func _set_cash(val):
	$Cash/Items/Label.text = str(val)
	cash = val
	
#func set_items(items)