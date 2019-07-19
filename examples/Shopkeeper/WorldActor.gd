extends Node

class_name WorldActor

export var _start_inv = {}
export var cash: int

onready var inventory = IterList.new()

func _init(name="", cash=0):
	self.cash = cash
	self.name = name
	
func _ready() -> void:	
	for k in _start_inv.keys():
		var inv = InventoryItem.new(k, _start_inv[k])
		inventory.items.append(inv)