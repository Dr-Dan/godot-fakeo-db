extends Node

"""
"""

const ButtonInvItem = preload("res://examples/Shopkeeper/scenes/ButtonInventoryItem.tscn")
const GLF = OperatorFactory
const List = EnumeratorsDeferred.ListEnumerator
const IType = WorldActor.ItemType
const Query = QueryBuilderDeferred

const PLAYER = "Player"
const SHOPKEEPER = "Shopkeeper"

##############################################################################

onready var item_data = List.new([
	{type=IType.MEAT, display_name="Meat", value=3, descr="Some meat."},
	{type=IType.AXE, display_name="Axe", value=25, descr="A simple logging axe."},
	{type=IType.SWORD, display_name="Sword", value=40, descr="A sword from some lost soldier."},
	{type=IType.SPELL_HEAL, display_name="Heal Spell", value=120, descr="Refills a substantial amount of HP."},
	{type=IType.SCROLL_FIRE, display_name="Fire Scroll", value=300, descr="Deals a small amount of fire damage.\nDestroyed on use."},
	{type=IType.THE_JUICE, display_name="JUICE", value=500, descr="Nice."},
])

onready var gui = $GUI

##############################################################################

export var shopkeeper_mult = 1.5

func _ready():
	update_gui()

##############################################################################

func update_gui():
	var p_panel = gui.get_panel(0)
	var s_panel = gui.get_panel(1)
	
	p_panel.display_name = PLAYER
	s_panel.display_name = SHOPKEEPER
	
	p_panel.cash = $Player.cash
	s_panel.cash = $Shopkeeper.cash
		
	update_actor_gui($Shopkeeper, s_panel)
	update_actor_gui($Player, p_panel)


func update_actor_gui(actor, container):
	clear_inventory_gui(container)

	for shp_item in actor.inventory.get_children():
		var btn = create_button(actor.shop_mult_bonus, shp_item)
		
		btn.connect("button_down", self, "item_clicked", [btn, actor, shp_item.type])
		container.get_node("Inventory").add_child(btn)

func clear_inventory_gui(container):
	for c in container.get_node("Inventory").get_children():
		c.queue_free()


func create_button(mult, shp_item):
	var info = item_data.where({type=GLF.eq(shp_item.type)}).at(0)	
	if info == null: return null
	var txt = "%s\nvalue:%d\namt %d" %\
		[info.display_name, get_cost(info.type) * mult, shp_item.amt]

	var btn = ButtonInvItem.instance()
	btn.get_node("Label").text = txt
	btn.hint_tooltip = info.descr
	return btn

func get_cost(type) -> int:
	var value = item_data.where({type=GLF.eq(type)}).at(0).value
	return int(value)
	
func item_clicked(btn, actor, type):
	if actor.name == PLAYER:
		if transfer_cash($Shopkeeper, $Player, get_cost(type)*actor.shop_mult_bonus):
			transfer_item($Player.inventory, $Shopkeeper.inventory, type)
	else:
		if transfer_cash($Player, $Shopkeeper, get_cost(type)*actor.shop_mult_bonus):
			transfer_item($Shopkeeper.inventory, $Player.inventory, type)
	update_gui()

##############################################################################

static func transfer_cash(source, target, amt:int) -> bool:
	if source.cash - amt >= 0:
		target.cash += amt
		source.cash -= amt
		return true
	return false
	
static func transfer_item(source, target, type) -> bool:
	if source.remove_item(type):
		target.deposit_item(type, 1)
		return true
	return false