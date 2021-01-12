with JEWL.Windows;
with Gnat.String_Split;
with Ada.Containers.Indefinite_Vectors;
with Ada.Text_Io;
with Ada.Strings.Unbounded;

Package sudoku_tools_16 Is
    Type Command_Code is (Quit, Load, Save, Finish, Show);
    package My_Windows is new JEWL.Windows (Command_Code);
    use My_Windows;
    use Gnat.String_Split;
    use Ada.Containers;
    package String_Vectors is new Indefinite_Vectors (Positive, String);
    use String_Vectors;
    use Ada.Strings.Unbounded;

    BOXES_SIZE: Integer := 4;
    LOOP_LENGTH: Integer := 16;
    BLOCK_WIDTH: Integer := 130;

    Type Grid_Panel_Type is array (1..BOXES_SIZE, 1..BOXES_SIZE) of Panel_Type;
    Type Block_Editbox_Type is array (1..BOXES_SIZE, 1..BOXES_SIZE) of Editbox_Type;
    Type Grid_Block_Type is array (1..BOXES_SIZE, 1..BOXES_SIZE) of Block_Editbox_Type;
    Type Matrix_Type is array (1..LOOP_LENGTH, 1..LOOP_LENGTH) of Integer;
    Type List_Type is array (1..LOOP_LENGTH) of Integer;

    Function getLine (grid: Matrix_Type; index: in Integer) return List_Type;
    Function getColumn (grid: Matrix_Type; index: in Integer) return List_Type;
    Function getBlock (grid: in Matrix_Type; index_i: in Integer; index_j: in Integer) return List_Type;
    Function isValidList (list: in List_Type) return Boolean;
    Function isValidGrid (grid: in Matrix_Type) return Boolean;
    Procedure showGrid (grid: in Matrix_Type);
    Procedure createGrid (window: in Frame_Type; grid_blocks: out Grid_Block_Type);
    Procedure createBlock (panel: in Panel_Type; block: out Block_Editbox_Type);
    Procedure setGrid (grid_blocks: in out Grid_Block_Type; grid: in Matrix_Type; base_filename: in out Unbounded_String);
    Function getGrid (grid_blocks: in Grid_Block_Type; grid: out Matrix_Type; saving: in Boolean) return Boolean;
    Function readFile (file: in Ada.Text_IO.File_Type) return Vector;
    Function saveGrid (grid: in Matrix_Type; filename: in String; base_filename: in out Unbounded_String) return Boolean;
    Function split (line: in String; list: out List_Type) return Boolean;
    Function fillGrid (lines: in vector; grid: out Matrix_Type; base_filename: in out Unbounded_String) return Boolean;
    Function isNumeric (value: in String) return Boolean;
    Function loadBaseMatrix(base_filename: in out Unbounded_String; grid: out Matrix_Type) return Boolean;

End sudoku_tools_16;
