extends Node

export var _name = ""
export var cash: int
export var start_inv = {}

onready var inventory = IterList.new()
	
# Called when the node enters the scene tree for the first time.
func _ready() -> void:	
	name = _name
	for k in start_inv.keys():
		var inv = InventoryItem.new(k, start_inv[k])
		inventory.items.append(inv)