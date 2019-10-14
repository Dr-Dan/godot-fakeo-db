extends MarginContainer

class_name ShopGUI

signal item_pressed(side, item_type)

const ButtonInvItem = preload("res://examples/Shopkeeper/scenes/ButtonInventoryItem.tscn")
const gui_base = "HBoxContainer"

const LEFT = 0
const RIGHT = 1

onready var r_panel = get_node(gui_base + "/ShopkeeperPanel")
onready var l_panel = get_node(gui_base + "/PlayerPanel")

#onready var panels = [l_panel, r_panel]
onready var panels = {
	LEFT:{panel=l_panel, name=""}, 
	RIGHT:{panel=r_panel, name=""}, 
	}
	
func get_panel(side):
	if side in panels:
		return panels[side].panel
		
func setup_panel(side, panel_name):
	panels[side].panel.display_name = panel_name
	
func update_actor_inventory(inventory, side, cost_strategy):
	var container = panels[side].panel
	clear_inventory_gui(container)

	for shp_item in inventory.to_list():
		var btn = create_item_button(shp_item.type, cost_strategy.eval(shp_item.type), shp_item.amt, shp_item.descr)
		btn.connect("button_down", self, "item_clicked", [side, shp_item.type])
		container.get_node("Inventory").add_child(btn)

func clear_inventory_gui(container):
	for c in container.get_node("Inventory").get_children():
		c.queue_free()
				
func create_item_button(type, cost, amt, description):
	var txt = "%s\nvalue:%d\namt %d" %\
		[type, cost, amt]

	var btn = ButtonInvItem.instance()
	btn.get_node("Label").text = txt
	btn.hint_tooltip = description
	return btn
				
func item_clicked(side, type):
	emit_signal("item_pressed", side, type)