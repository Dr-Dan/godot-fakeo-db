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
	table.headings = headings
	cltn = data
	
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
	var d = validate_cell(val, col)
	if d[0]:
		if col < headings.size():
			cltn.at(row-2)[headings[col].name] = d[1]
			table.set_cell(col, row, val)
			emit_signal("changed")

func validate_cell(text, col):
	var type = headings[col].type
	var val = text
	var valid = false
	match type:
		TYPE_STRING:
			valid = true
		TYPE_INT:
			if val.is_valid_integer():
				val = val.to_int()
				valid = true
		TYPE_REAL:
			if val.is_valid_float():
				val = val.to_float()			
				valid = true
		TYPE_VECTOR2:
			if val.match("(*,*)"):
#				val = str2var("Vector2"+val)
#				if not val is String:
#					valid = true
				var v = str2vec(val)
				if v != null:
					val = v
					valid = true
	return [valid, val]

func str2vec(s:String):
	var r = s.substr(1, s.length()-2).split(",")
	if r.size() == 2:
		for n in r:
			if not n.is_valid_float():
				return null
		return Vector2(r[0], r[1])
	return null
