extends Node
class_name InventoryItem

export (WorldActor.ItemType) var type = WorldActor.ItemType.NONE
export (int) var amt

func setup(type, amt):
	self.type = type
	self.amt = amt
	return self