extends Control

signal changed

const fdb = preload("res://addons/fakeo_db/scripts/FakeoDB.gd")
const ops = fdb.OperatorFactory

export (NodePath) var table_path
onready var table = get_node(table_path)

var headings:Array
var cltn:Array
var heading2idx={}
func setup(headings:Array, data:Array):
	self.headings = headings
	for h in headings.size():
		heading2idx[headings[h].name] = h
	cltn = data
	table.headings = headings
	
	table.v_sep = 0
	table.h_sep = 0
	table.ignore_rows = 2
	table.min_cell_width = 60
	
	setup_tbl(_get_headings(), cltn)
	table.connect("on_cell_edited", self, "_update_list")
			
func _update_table(item=null):
	table.clear_edited()
	setup_tbl(_get_headings(), cltn)

func _get_headings():
	var result = []
	for h in headings:
		result.append(h.name)
	return result
	
func setup_tbl(headings, data):
	var tbl_data = data
	tbl_data.push_front([])
	tbl_data.push_front(headings)
	table.from_data(tbl_data)
	table.get_row(0).modulate=Color.lightblue

func _update_list(row, col, val):
	if col < headings.size()-1:
		cltn[row-2][heading2idx[headings[col].name]] = val
		emit_signal("changed")
