extends Control

const fdb = preload("res://addons/fakeo_db/scripts/FakeoDB.gd")
const ops = fdb.OperatorFactory

var data = fdb.list([
	{name="Wooden Sword", type="melee", subtype="one-handed", dmg=6, atk_rate=1.0,atk_range=10.0},
	{name="John's Rock", type="ranged", subtype="thrown", dmg=21, atk_rate=0.5, atk_range=10.0, firing_rate=0.5},
	{name="Wooden Bow", type="ranged", subtype="bow", dmg=4, atk_rate=0.7, atk_range=20.0, firing_rate=1.4},
	{name="Glass Bow", type="ranged", subtype="bow", dmg=7, atk_rate=0.8, atk_range=20.0, firing_rate=1.7},
	{name="Ancient Bow", type="ranged", subtype="bow", dmg=10, atk_rate=0.7, atk_range=30.0, firing_rate=1.0},
])

onready var table = $MarginContainer/Panel/Table

var headings = ["name", "subtype", "dmg", "firing_rate", "DPS"]

func _ready() -> void:
	table.v_sep = 0
	table.h_sep = 0
	setup_tbl(data, headings)
	table.connect("on_cell_edited", self, "_update_list")
	
func _dps(item):
	item["DPS"] = item.dmg*item.atk_rate
	return item
	
func setup_tbl(list, headings):
	var projected = list.select(self, "_dps").project(headings)
	var kv = to_keys_and_values(projected, headings)
	var tbl_data = kv.values
#	table.add_row_data(headings)
#	for t in tbl_data:
#		table.add_row_data(t)
#	table.inflate()
	tbl_data.push_front([])
	tbl_data.push_front(kv.keys)
	table.from_data(tbl_data)

func to_keys_and_values(list, headings, default="-"):
	var items = []
	for result in list:
		items.append([])
		for h in headings:
			if h in result:
				items.back().append(result[h])
			else:
				items.back().append(default)
	return {keys=headings, values=items}

func _update_list(row, col, val):
	# subtract header offset
	data.at(row-2)[headings[col]] = val
