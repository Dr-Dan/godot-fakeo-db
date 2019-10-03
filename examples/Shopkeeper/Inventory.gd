extends Node

class_name Inventory

var items = [] setget ,_get_items

func _get_items():
	return get_children()