extends Node

const ButtonInvItem = preload("../scenes/ButtonInventoryItem.tscn")
const GLF = OperatorFactory
const List = EnumeratorsDeferred.ListEnumerator
const Query = QueryBuilderDeferred

const IType = InventoryItem.ItemType

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
	{type=IType.GOLD_PLATE, display_name="Golden Plate", value=200, descr="An ornamental gold plate"},
])

onready var gui = $GUI

##############################################################################

export var shopkeeper_mult = 1.5
var btn_factory = ButtonFactory.new()

onready var player = $Actors/Player
onready var shopkeeper = $Actors/Shopkeeper

##############################################################################

func _ready():
	update_gui()

##############################################################################

func update_gui():
	var p_panel = gui.panels.left
	var s_panel = gui.panels.right
	
	p_panel.display_name = PLAYER
	s_panel.display_name = SHOPKEEPER
	
	p_panel.cash = player.cash
	s_panel.cash = shopkeeper.cash
		
	update_actor_gui(shopkeeper, s_panel)
	update_actor_gui(player, p_panel)
		
func update_actor_gui(actor, container):
	container.clear_inventory()
	var btns = actor.inventory.to_enumerator().select(funcref(self, "select_as_btn"), actor)
	for btn in btns:
		container.inventory.add_child(btn)

func select_as_btn(shp_item, actor):
	var info = item_data.first({type=GLF.eq(shp_item.type)})
	var cost = info.value * actor.shop_mult_bonus
	var btn = btn_factory.create_button_inv_item(info.display_name, shp_item.amt, cost, info.descr)
	btn.connect("button_down", self, "item_clicked", [actor.name, shp_item.type, cost])
	return btn

func item_clicked(owner_name, type, cost):
	match owner_name:
		PLAYER:
			if transfer_cash(shopkeeper, player, cost):
				transfer_item(player.inventory, shopkeeper.inventory, type)
		SHOPKEEPER:
			if transfer_cash(player, shopkeeper, cost):
				transfer_item(shopkeeper.inventory, player.inventory, type)
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