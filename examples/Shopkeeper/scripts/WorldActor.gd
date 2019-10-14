extends Node

class_name WorldActor

enum ItemType{
	NONE,
	MEAT,
	AXE,
	SWORD,
	SPELL_HEAL,
	SCROLL_FIRE,
	THE_JUICE,
}

export var cash: int
export var shop_mult_bonus: float = 1.0

onready var inventory = $Inventory

func _init(name="", cash=0):
	self.cash = cash
	self.name = name