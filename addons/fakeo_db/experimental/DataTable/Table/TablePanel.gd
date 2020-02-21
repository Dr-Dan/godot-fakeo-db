extends VBoxContainer

const TableRow = preload("TableRow.gd")
const Lbl = preload("res://addons/fakeo_db/experimental/DataTable/Label.tscn")

export var h_sep = 4 setget set_h_separation
export var v_sep = 4 setget set_v_separation

var n_rows = 0
var n_cols = 0

var widths = []
var min_cell_height = 20
var min_cell_width = 40

func set_v_separation(sep):
	set("custom_constants/separation", sep)
	v_sep = sep

func set_h_separation(sep):
	for y in n_rows:
		var row = get_child(y)
		row.h_sep = sep
	h_sep=sep

func from_data(data=[], inflate=true):
	clear()
	var y = 0
		
	for row in data:
		if row.size() > n_cols: n_cols = row.size()
			
	var cols = -1
	if inflate:
		cols = n_cols
	for row in data:
		add_row_data(row, cols)
		
	inflate()
		
func clear():
	n_rows = 0
	n_cols = 0
	for i in range(get_child_count()-1, -1, -1):
		get_child(i).free()
	
func set_dimensions(cols=0, rows=0):
	var dr = rows-n_rows
	if dr > 0:
		for i in dr:
			add_row()
	var dc = cols-n_cols
	if dc > 0:
		for i in dc:
			widths.append(min_cell_width)
	n_rows = rows
	n_cols = cols
	detect()
	inflate()

# TODO: set_cell_expand? or expand argument
func set_cell(x:int, y:int, value):
	if index_exists(x,y):
		get_child(y).get_child(x).text = str(value)
		return true
	return false

func get_cell(x:int, y:int):
	if index_exists(x,y):
		return get_row(y).get_child(x)
	return null
	
func get_row(y):
	if y < n_rows:
		return get_child(y)
	return null

func index_exists(x,y):
	return x >= 0 and y >= 0 and x < n_cols and y < n_rows
	

func add_row() -> TableRow:
	var data = []
	for i in n_cols:
		data.append("")
	return add_row_data([], true)
	
func add_row_data(data, auto_fill=true) -> TableRow:
	var row = TableRow.new()
	add_child(row)
	n_rows += 1
	for d in data:
		var l = create_label(d)
		row.add_cell(l)
	
	var diff = n_cols-data.size()
	if diff < 0:
		n_cols = data.size()
	if auto_fill and diff > 0:
		for d in range(diff):
			var l = create_label()
			row.add_cell(l)
	row.h_sep = h_sep
	detect_row(row)
	return row
	
func detect():
	for r in n_rows:
		detect_row(get_row(r))

func detect_row(row):
	var N = row.get_child_count()
	var d = N - widths.size()
	if d > 0:
		for i in d:
			widths.append(min_cell_width)
	for x in N:
		var itm = row.get_children()[x]
		if itm.rect_size.x > widths[x]:
			widths[x] = itm.rect_size.x
	return N
	
func inflate():
	for y in n_rows:
		var row = get_row(y)
		inflate_row(row)
				
		
func inflate_row(row):
	for x in n_cols:
		if x >= row.get_child_count():
			var l = create_label()
			row.add_cell(l)
		var itm = row.get_children()[x]
		itm.rect_min_size.x = widths[x]
		itm.rect_size.x = widths[x]
			
func create_label(value="", align=Label.ALIGN_CENTER):
	var l = Lbl.instance()
	l.mouse_filter = Control.MOUSE_FILTER_IGNORE
	l.size_flags_horizontal = SIZE_EXPAND_FILL
	l.rect_min_size=Vector2(min_cell_width, min_cell_height)
	l.align = align
	l.text = str(value)
	return l
