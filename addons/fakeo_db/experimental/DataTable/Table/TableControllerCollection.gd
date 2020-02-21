extends Control

signal changed

const fdb = preload("res://addons/fakeo_db/scripts/FakeoDB.gd")
const ops = fdb.OperatorFactory

# TODO: make table and set table gui_root to self
export (NodePath) var table_path
onready var table = get_node(table_path)

var headings:Array
var cltn:fdb.Enumerables.Collection

func setup(headings, data:fdb.Enumerables.Collection):
	self.headings = headings
	cltn = data
	table.headings = headings
	
	table.v_sep = 0
	table.h_sep = 0
	table.ignore_rows = 2
	table.min_cell_width = 60
	
	setup_tbl(_get_headings(), cltn)
	table.connect("on_cell_edited", self, "_update_list")
	cltn.connect("on_item_added", self, "_update_table")
	cltn.connect("on_item_erased", self, "_update_table")
			
func _update_table(item=null):
	table.clear_edited()
	setup_tbl(_get_headings(), cltn)

func _get_headings():
	var result = []
	for h in headings:
		result.append(h.name)
	return result
	
func setup_tbl(headings, data):
	var projected = data.project_values(headings, "-")
	var tbl_data = projected.to_array()
	tbl_data.push_front([])
	tbl_data.push_front(headings)
	table.from_data(tbl_data)
	table.get_row(0).modulate=Color.lightblue

func _update_list(row, col, val):
	if col < headings.size():
		cltn.at(row-2)[headings[col].name] = val
		emit_signal("changed")
