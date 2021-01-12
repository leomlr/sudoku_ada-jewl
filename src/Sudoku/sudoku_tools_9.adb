with JEWL.IO;
with ada.Integer_Text_IO;   -- Tell compiler to use i/o library
with Ada.Strings.Fixed;
use ada.Integer_Text_IO;   -- Use library routines w/o fully qualified names

Package Body sudoku_tools_9 Is

    -- Function: Show a grid in console.
    Procedure showGrid (grid: in Matrix_Type) Is
        Begin
            Ada.Text_IO.Put_Line("Current Matrix:");
            for i in 1..LOOP_LENGTH loop
                for j in 1..LOOP_LENGTH loop
                    if (grid(i,j) > 64 and grid(i,j) < 72) or (grid(i,j) > 96 and grid(i,j) < 104) then
                        Ada.Text_IO.Put(" " & Character'Val(grid(i,j)));
                    else
                        Ada.Text_IO.Put(Integer'Image(grid(i,j)));
                    end if;
                    if j = BOXES_SIZE or j = 2*BOXES_SIZE or j = 3*BOXES_SIZE then
                        Ada.Text_IO.Put("  ");
                    end if;
                end loop;
                if i = BOXES_SIZE or i = 2*BOXES_SIZE or i = 3*BOXES_SIZE then 
                    Ada.Text_IO.new_line;
                end if;
                Ada.Text_IO.new_line;
            end loop;
            Ada.Text_IO.new_line;
    End showGrid;

    -- Function: Get grid Line.
    Function getLine (grid: Matrix_Type; index: in Integer) return List_Type Is
        list: List_Type;
        Begin
            for i in 1..LOOP_LENGTH loop
                list(i) := grid(index, i);
            end loop;
            return list;
    End getLine;

    -- Function: Get grid column.
    Function getColumn (grid: Matrix_Type; index: in Integer) return List_Type Is
        list: List_Type;
        Begin
            for i in 1..LOOP_LENGTH loop
                list(i) := grid(i, index);
            end loop;
            return list;
    End getColumn;

    -- Function: Get grid block.
    Function getBlock (grid: in Matrix_Type; index_i: in Integer; index_j: in Integer) return List_Type Is
        list: List_Type;
        index: Integer;
        start_i: Integer;
        start_j: Integer;
        Begin                                     --revoir la formule des getblok pour le 16
            index := 1;
            start_i := (index_i-1)*BOXES_SIZE + 1;                  
            start_j := (index_j-1)*BOXES_SIZE + 1;
            for i in start_i..(start_i + 2) loop
                for j in start_j..(start_j + 2) loop
                   list(index) := grid(i, j);
                   index := index + 1;
                end loop;
            end loop;
            return list;
    End getBlock;

    -- Function: Check a list.
    Function isValidList (list: in List_Type) return Boolean Is
        Begin
            for i in 1..LOOP_LENGTH loop
                for j in 1..LOOP_LENGTH loop
                    if not (i = j) then
                        if list(i) = list(j) then
                            return False;
                        end if;
                    end if;
                end loop;
            end loop;
            return True;
    End isValidList;

    -- Function: Validate a grid;
    Function isValidGrid (grid: in Matrix_Type) return Boolean Is
        listToCheck: List_Type;
        Begin
            -- Checking Lines
            for i in 1..LOOP_LENGTH loop
                listToCheck := getLine(grid, i);
                if not isValidList(listToCheck) then 
                    return False;
                else 
                    Ada.Text_IO.Put_Line("Line " & Integer'Image(i) & " is Ok !");
                end if;
            end loop;
            -- Checking Columns
            for i in 1..LOOP_LENGTH loop
                listToCheck := getColumn(grid, i);
                if not isValidList(listToCheck) then 
                    return False;
                else 
                    Ada.Text_IO.Put_Line("Column " & Integer'Image(i) & " is Ok !");
                end if;
            end loop;
            -- Checking Blocks
            for i in 1..BOXES_SIZE loop
                for j in 1..BOXES_SIZE loop
                    listToCheck := getBlock(grid, i, j);
                    if not isValidList(listToCheck) then 
                        return False;
                    else 
                        Ada.Text_IO.Put_Line("Block (" & Integer'Image(i) & "," & Integer'Image(j) & ") is Ok !");
                    end if;
                end loop;
            end loop;
            -- Return True (grid is correct)
            return True;
    End isValidGrid;

    Procedure createGrid (window: in Frame_Type; grid_blocks: out Grid_Block_Type) Is
        panels: Grid_Panel_Type;
        Begin
            for i in 1..BOXES_SIZE loop
                for j in 1..BOXES_SIZE loop
                    panels(i, j) := Panel (window, (20+(j-1)*BLOCK_WIDTH, 20+(i-1)*BLOCK_WIDTH), BLOCK_WIDTH, BLOCK_WIDTH); -- changer la formule
                    createBlock(panels(i, j), grid_blocks(i, j));
                end loop;
            end loop;
    End createGrid;

    Procedure createBlock (panel: in Panel_Type; block: out Block_Editbox_Type) Is
        Begin
            for i in 1..BOXES_SIZE loop
                for j in 1..BOXES_SIZE loop
                    block(i, j) := Editbox (panel, (10+(j-1)*30, 10+(i-1)*30), 20, 20, " 0", False, Font ("Montserrat", 10)); -- changer la formule de recup
                end loop;
            end loop;
    End createBlock;

    Function getGrid (grid_blocks: in Grid_Block_Type; grid: out Matrix_Type; saving: in Boolean) return Boolean Is
        block: Block_Editbox_Type;
        editbox: Editbox_Type;
        value: Integer;
        x_index: Integer;
        y_index: Integer;
        Begin
            for i in 1..BOXES_SIZE loop
                for j in 1..BOXES_SIZE loop
                    for k in 1..BOXES_SIZE loop
                        block := grid_blocks(i, k);
                        for l in 1..BOXES_SIZE loop
                            editbox := block(j, l);
                            x_index := (i-1)*BOXES_SIZE+j;
                            y_index := (k-1)*BOXES_SIZE+l;
                            if isNumeric(Get_Text(editbox)) then
                                value := Integer'Value(Get_Text(editbox));
                                if not saving then
                                    if value /= 0 then
                                        grid(x_index, y_index) := value;
                                    else
                                        return False;
                                    end if;
                                else
                                    grid(x_index, y_index) := value;
                                end if;
                            else 
                                if LOOP_LENGTH = 16 then
                                    for i in 1..10 loop
                                        value := Character'Pos(Get_Text(editbox)(i));
                                        if value /= 32 then
                                            if Get_Text(editbox)'Length > i then
                                                if Character'Pos(Get_Text(editbox)(i+1)) /= 32 then
                                                    return False;
                                                end if;
                                            end if;
                                            exit;
                                        end if;
                                    end loop;
                                    if (value > 64 and value < 72) or (value > 96 and value < 104) then
                                        grid(x_index, y_index) := value;
                                    else
                                        return False;
                                    end if;
                                else 
                                    return False;
                                end if;
                            end if;
                        end loop;
                    end loop;
                end loop;
            end loop;
            return True;
    End getGrid;

    Procedure setGrid (grid_blocks: in out Grid_Block_Type; grid: in Matrix_Type; base_filename: in out Unbounded_String) Is
        block: Block_Editbox_Type;
        editbox: Editbox_Type;
        value: Integer;
        base_grid: Matrix_Type;
        base_loaded: Boolean := True;
        Begin
            if base_filename /= "" then
                base_loaded := loadBaseMatrix(base_filename, base_grid);
            end if;
            if base_loaded then
                for i in 1..BOXES_SIZE loop
                    for j in 1..BOXES_SIZE loop
                        for k in 1..BOXES_SIZE loop
                            block := grid_blocks(i, k);
                            for l in 1..BOXES_SIZE loop
                                value := grid((i-1)*BOXES_SIZE+j, (k-1)*BOXES_SIZE+l);
                                editbox := block(j, l);
                                if (value > 64 and value < 72) or (value > 96 and value < 104) then
                                    Set_Text(editbox, " " & Character'Val(value));
                                else 
                                    Set_Text(editbox, Integer'Image(value));
                                end if;
                                if value /= 0 then
                                    if base_filename /= "" then
                                        if base_grid((i-1)*BOXES_SIZE+j, (k-1)*BOXES_SIZE+l) /= 0 then
                                            Disable(editbox);
                                        else
                                            Enable(editbox);
                                        end if;
                                    else
                                        Disable(editbox);
                                    end if;
                                else 
                                    Enable(editbox);
                                end if;
                            end loop;
                        end loop;
                    end loop;
                end loop;
            end if;
    End setGrid;

    Function loadBaseMatrix(base_filename: in out Unbounded_String; grid: out Matrix_Type) return Boolean Is
        file: Ada.Text_IO.File_Type;
        open_error: Boolean := False;
        Begin
            Begin
                Ada.Text_IO.Open(file, Ada.Text_IO.IN_FILE, To_String(base_filename));
            Exception
                when others =>
                    Ada.Text_IO.Put_Line("Text_IO FileOpeningError: File not found.");
                    open_error := True;
            End;
            if not open_error then
                if fillGrid(readFile(file), grid, base_filename) then
                    Ada.Text_IO.Close(file);
                    return True;
                end if;
            end if;
            return False;
    End loadBaseMatrix;

    Function readFile (file: in Ada.Text_IO.File_Type) return Vector Is
        lines: Vector := Empty_Vector;
        Begin
            while not Ada.Text_IO.End_Of_File(file) loop
                lines.Append(Ada.Text_IO.Get_Line(file));
            end loop;
            return lines;
    End readFile;

    Function saveGrid (grid: in Matrix_Type; filename: in String; base_filename: in out Unbounded_String) return Boolean Is
        file: Ada.Text_IO.File_Type;
        line: Unbounded_String;
        Begin
            Ada.Text_IO.Create(file, Ada.Text_IO.Out_File, filename & ".txt");
            Ada.Text_IO.Put_Line(To_String(base_filename));
            JEWL.IO.Put_Line(file, To_String(base_filename));
            for i in 1..LOOP_LENGTH loop
                line := To_Unbounded_String("");
                for j in 1..LOOP_LENGTH loop
                    if (grid(i,j) > 64 and grid(i,j) < 72) or (grid(i,j) > 96 and grid(i,j) < 104) then
                        line := line & Character'Val(grid(i,j));
                    else
                        line := line & Integer'Image(grid (i, j))(2);
                    end if;
                    if j /= LOOP_LENGTH then 
                        line := line & " ";
                    end if;
                end loop;
                JEWL.IO.Put_Line(file, To_String(line));
            end loop;
            Ada.Text_IO.Close(file);
            return True;
    End saveGrid;

    Function split (line: in String; list: out List_Type) return Boolean Is
        subs : Slice_Set;
        seps : constant String := " ";
        index: Integer;
        value: Integer;
        item: String (1..1);
        Begin
            Ada.Text_IO.Put_Line(line);
            Create (S => subs, From => line, Separators => seps, Mode => Multiple);
            for i in 1 .. Slice_Count(subs) loop
                item := Slice (subs, i);
                index := Integer'Value(Slice_Number'Image(i));
                if isNumeric(item) then
                    value := Integer'Value(item);
                    list(index) := value;
                else 
                    value := Character'Pos(item(1));
                    if (value > 64 and value < 72) or (value > 96 and value < 104) then
                        list(index) := value;
                    else
                        return False;
                    end if;
                end if;
            end loop;
            return True;
    End split;

    Function fillGrid (lines: in vector; grid: out Matrix_Type; base_filename: in out Unbounded_String) return Boolean Is
        list: List_Type;
        len: Integer := Integer'Value(Count_Type'Image(lines.Length));
        Begin
            if (len = LOOP_LENGTH + 1) or (len = LOOP_LENGTH + 2) then
                if (Ada.Strings.Fixed.Index(lines(1), ".txt", 1) > 0) then
                    base_filename := To_Unbounded_String(lines(1));
                    Ada.Text_IO.Put_Line(To_String(base_filename));
                else
                    return False;
                end if;                    
                for i in 1..LOOP_LENGTH loop
                    if split(lines(i+1), list) then
                        for j in 1..LOOP_LENGTH loop
                            grid(i, j) := list(j);
                        end loop;
                    else
                        return False;
                    end if;
                end loop;
            else
                return False;
            end if;
            return True;
    End fillGrid;

    Function isNumeric (value: in String) return Boolean Is
        dummy : Float;
        Begin
            dummy := Float'Value(value);
            return True;
        Exception
            when others =>
                return False;
    End isNumeric;

End sudoku_tools_9;
