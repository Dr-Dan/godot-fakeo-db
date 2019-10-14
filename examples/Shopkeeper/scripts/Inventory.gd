extends Node

class_name Inventory

export var items = [] setget ,_get_items

func _enter_tree():
	items = _get_items()
	
func _get_items():
	return get_children()

func to_enumerator():
	return EnumeratorsDeferred.ListEnumerator.new(_get_items())
	
func deposit_item(type, amt=1):
	var item = to_enumerator()\
	.where({type=OperatorFactory.eq(type)})\
	.at(0)
	
	if item != null:
		item.amt += amt
	else:
		add_child(InventoryItem.new().setup(type, 1))

func remove_item(type):
	var item = to_enumerator()\
	.where({type=OperatorFactory.eq(type)})\
	.at(0)

	if item != null:
		item.amt -= 1
		if item.amt <= 0:
			item.free()
		return true
	return false