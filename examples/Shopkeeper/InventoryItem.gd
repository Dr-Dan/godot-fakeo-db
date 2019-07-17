extends Resource
class_name InventoryItem

var type: String
var amt: int

func _init(type, amt=1):
	self.type = type
	self.amt = amt