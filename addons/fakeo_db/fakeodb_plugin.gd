tool
extends EditorPlugin

const LIB_NAME = 'FakeoDB'
const SCRIPTS_PATH = 'res://addons/fakeo_db/scripts/'

func _enter_tree():
	self.add_autoload_singleton(LIB_NAME, SCRIPTS_PATH+LIB_NAME+".gd")

func _exit_tree():
	self.remove_autoload_singleton(LIB_NAME)