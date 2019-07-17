extends Node

"""
"""

const ButtonInvItem = preload("res://examples/Shopkeeper/ButtonInventoryItem.tscn")
const GLF = Factory

class Actor:
	var name: String
	var cash: int
	var inventory
	
	func _init(name, cash, inventory):
		self.name = name
		self.cash = cash
		self.inventory = inventory
		
class InventoryItem:
	var type: String
	var amt: int

	func _init(type, amt=1):
		self.type = type
		self.amt = amt
		
const MEAT = "Meat"
const AXE = "Axe"
const SWORD = "Sword"
const SPELL_HEAL = "Heal Spell"
const THE_JUICE = "##JUICE##"

const PLAYER = "Player"
const SHOPKEEPER = "Shopkeeper"

onready var item_data = IterList.new([
	{type=MEAT, value=3, descr="Some meat."},
	{type=AXE, value=25, descr="A simple logging axe."},
	{type=SWORD, value=40, descr="A sword from some lost soldier."},
	{type=SPELL_HEAL, value=120, descr="Refills a substantial amount of HP."},
	{type=THE_JUICE, value=1000, descr="Nice."},
])

onready var shopkeep_inventory = IterList.new([
	InventoryItem.new(MEAT),
	InventoryItem.new(AXE, 2),
	InventoryItem.new(SPELL_HEAL, 1),
	InventoryItem.new(THE_JUICE, 1),
	])
	
onready var player_inventory = IterList.new([
	InventoryItem.new(MEAT, 5),
	InventoryItem.new(SWORD, 2),
	])

onready var actor_data = IterList.new([
	Actor.new(PLAYER, 180, player_inventory),
	Actor.new(SHOPKEEPER, 800, shopkeep_inventory),
	])

export var shopkeeper_mult = 1.5


func _ready():
	update_gui()
	
func update_gui():
	update_actor_gui(SHOPKEEPER, $GUI/MarginContainer/HBoxContainer/ShopkeeperPanel)
	update_actor_gui(PLAYER, $GUI/MarginContainer/HBoxContainer/PlayerPanel)

	update_cash_display(SHOPKEEPER, $GUI/MarginContainer/HBoxContainer/ShopkeeperPanel)
	update_cash_display(PLAYER, $GUI/MarginContainer/HBoxContainer/PlayerPanel)

func update_actor_gui(name, container):
	clear_inventory_gui(container)
		
	var actor = actor_data.first({name=GLF.eq(name)})
	if actor != null:
		for shp_item in actor.inventory.to_list():
			var btn = create_button(name, shp_item)
			btn.connect("button_down", self, "item_clicked", [btn, name, shp_item.type])
			container.get_node("Inventory").add_child(btn)
		
func clear_inventory_gui(container):
	for c in container.get_node("Inventory").get_children():
		c.queue_free()
		
func update_cash_display(name, container):
	var actor = actor_data.first({name=GLF.eq(name)})
	container.get_node("Cash/Items/Label").text = str(actor.cash)

func create_button(name, shp_item):
	var info = item_data.first({type=GLF.eq(shp_item.type)})
	var txt = "%s\nvalue:%d\namt %d" %\
		[shp_item.type, get_cost(name, shp_item.type), shp_item.amt]

	var btn = ButtonInvItem.instance()
	btn.get_node("Label").text = txt
	btn.hint_tooltip = info.descr
	return btn

func item_clicked(btn, name, type):
	if name == PLAYER:
		if transfer_cash(SHOPKEEPER, PLAYER, get_cost(PLAYER, type)):
			deposit_item(SHOPKEEPER, type, 1)
			remove_item(PLAYER, type)
	else:
		if transfer_cash(PLAYER, SHOPKEEPER, get_cost(SHOPKEEPER, type)):
			deposit_item(PLAYER, type, 1)
			remove_item(SHOPKEEPER, type)
	update_gui()
	
func transfer_cash(source_name, target_name, amt) -> bool:
	var target = actor_data.first({name=GLF.eq(target_name)})
	var source = actor_data.first({name=GLF.eq(source_name)})
	if source.cash - amt >= 0:
		target.cash += amt
		source.cash -= amt
		return true
	return false
		
func get_cost(source, type) -> int:
	var value = item_data.first({type=GLF.eq(type)}).value
	if source == SHOPKEEPER:
		return int(value * shopkeeper_mult)
	return int(value)
	
func deposit_item(name, type, amt=1):
	var inv = actor_data.first({name=GLF.eq(name)})
	var i = inv.inventory.first({type=GLF.eq(type)})
	if i != null:
		i.amt += amt
	else:
		inv.inventory.items.append(InventoryItem.new(type, 1))
		
func remove_item(name, type):
	var actor = actor_data.first({name=GLF.eq(name)})
	var item = actor.inventory.first({type=GLF.eq(type)})
	
	item.amt -= 1
	if item.amt <= 0:
		actor.inventory.items.erase(item)
	var value = item_data.first({type=GLF.eq(type)}).value
			