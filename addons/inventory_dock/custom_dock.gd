tool
extends EditorPlugin

const InvPanel = preload("InventoryPanel.tscn")
const InvItem = preload("ButtonInventoryItem.tscn")
# A class member to hold the dock during the plugin lifecycle
var dock
#var items = []
var selected

func _enter_tree():
    # Initialization of the plugin goes here
    # Load the dock scene and instance it
	dock = InvPanel.instance()
	get_editor_interface().get_selection().connect("selection_changed", self, "_selection_changed")
	
    # Add the loaded scene to the docks
	add_control_to_dock(DOCK_SLOT_LEFT_UL, dock)
    # Note that LEFT_UL means the left of the editor, upper-left dock
	
var IType = InventoryItem.ItemType
func _selection_changed():
	clear_inventory_gui()
	
	var items = get_editor_interface().get_selection().get_selected_nodes()
	if items.size() == 1:
		var item = items.front()
		if item is Inventory:
			for i in item.items:
				var btn = InvItem.instance()
				var name = str(IType.keys()[i.type])
				var s = "%s\n%d" % [name, i.amt]
				btn.get_node("Label").text = s
				dock.get_node("Inventory").add_child(btn)

func _exit_tree():
    # Clean-up of the plugin goes here
    # Remove the dock
	remove_control_from_docks(dock)
#	items = []
     # Erase the control from the memory
	selected = null
	dock.free()
	

#func update_actor_inventory(inventory, cost_strategy):
#	var container = dock
#	clear_inventory_gui(container)
#
#	for shp_item in inventory.to_list():
#		var btn = create_item_button(shp_item.type, shp_item.amt, shp_item.descr)
#		container.get_node("Inventory").add_child(btn)
#
func clear_inventory_gui():
	for c in dock.get_node("Inventory").get_children():
		c.queue_free()
#
#func create_item_button(type, amt, description):
#	var txt = "%s\namt %d" %\
#		[type, amt]
#
#	var btn = InvItem.instance()
#	btn.get_node("Label").text = txt
#	btn.hint_tooltip = description
#	return btn
#