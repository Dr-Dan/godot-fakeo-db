extends Node

const InventoryItem = preload("InventoryItem.gd")

export var items = [] setget ,_get_items

func _enter_tree():
	items = _get_items()
	
func _get_items():
	return get_children()

func to_enumerator():
	return EnumeratorsDeferred.ListEnumerator.new(_get_items())
	
func deposit_item(type, amt=1):
	var item = to_enumerator()\
	.first({type=OperatorFactory.eq(type)})
	
	if item != null:
		item.amt += amt
	else:
		add_child(InventoryItem.new().setup(type, 1))

func remove_item(type):
	var item = to_enumerator()\
	.first({type=OperatorFactory.eq(type)})

	if item != null:
		item.amt -= 1
		if item.amt <= 0:
			item.free()
		return true
	return false
	
func clear():
	for c in _get_items():
		c.queue_free()