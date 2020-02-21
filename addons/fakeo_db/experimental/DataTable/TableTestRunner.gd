extends Control

const fdb = preload("res://addons/fakeo_db/scripts/FakeoDB.gd")
const ops = fdb.OperatorFactory

onready var data = fdb.cltn([
	{wpn_name="Wooden Sword", type="melee", subtype="one-handed", dmg=6, atk_rate=1.0,atk_range=10.0},
	{wpn_name="John's Rock", type="ranged", subtype="thrown", dmg=21, atk_rate=0.5, atk_range=10.0},
	{wpn_name="Wooden Bow", type="ranged", subtype="bow", dmg=4, atk_rate=0.7, atk_range=20.0},
	{wpn_name="Glass Bow", type="ranged", subtype="bow", dmg=7, atk_rate=0.8, atk_range=20.0},
	{wpn_name="Ancient Bow", type="ranged", subtype="bow", dmg=10, atk_rate=0.7, atk_range=30.0},
	$GodotPerson,
])

onready var person: fdb.Enumerables.Collection
onready var table_cntrl = $MarginContainer/Panel/ScrollContainer/MarginContainer
onready var table_cntrl2 = $MarginContainer2/Panel/ScrollContainer/MarginContainer

#var headings = ["wpn_name", "type", "subtype", "dmg", "atk_rate"]
# TODO: queries/expressions
var headings = [
	{name="wpn_name", type=TYPE_STRING},
	{name="type", type=TYPE_STRING},
	{name="subtype", type=TYPE_STRING},
	{name="dmg", type=TYPE_INT},
	{name="atk_rate", type=TYPE_REAL}]

var headings2 = [
	{name="wpn_name", type=TYPE_STRING},
#	{name="type", type=TYPE_STRING},
#	{name="subtype", type=TYPE_STRING},
	{name="dmg", type=TYPE_INT},
	{name="atk_rate", type=TYPE_REAL},
	{name="x", type=TYPE_REAL},
	{name="y", type=TYPE_REAL},
	]
	
func _ready() -> void:
	person = fdb.cltn([$GodotPerson])
#	person = data.take(3).to_collection()
#	data.first({wpn_name=""})
#	person.append($GodotPerson)
	table_cntrl.setup(headings, data)
	table_cntrl2.setup(headings2, person)
	table_cntrl.connect("changed", table_cntrl2, "_update_table")
	table_cntrl2.connect("changed", table_cntrl, "_update_table")
#	var expression = Expression.new()
#	expression.parse("dmg*atk_rate", ["val", "dmg", "atk_rate"])
#	print(expression.execute([1, 1, 22]))
#	var expression = Expression.new()
#	expression.parse('data.first({"wpn_name":"Glass Bow"})', ["data"])
#	print(expression.execute([data]))
