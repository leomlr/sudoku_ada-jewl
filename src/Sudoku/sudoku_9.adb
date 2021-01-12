------------------------------------------------------------------------------
--                                                                          --
--                   S U D O K U    W I T H     J E W L                     --
--                                                                          --
------------------------------------------------------------------------------

-- Taliesin Meillier

-- Add a John English's Window Library (JEWL).
with JEWL.IO;
-- Add the Ada Text Inputs/Outputs library.
with Ada.Text_Io;
with Ada.Strings.Unbounded;
with Ada.Strings.Fixed;
-- Add personal libraries.
with sudoku_tools_9;

Procedure sudoku_9 Is
    use JEWL.IO;
    use Ada.Strings.Unbounded;
    use sudoku_tools_9;
    
    use My_Windows;

    -- Frame parameters
    FRAME_TITLE: String := "Sudoku_9x9";
    FRAME_WIDTH: Integer := 455;
    FRAME_HEIGTH: Integer := 500;

    -- Memo parameters
    MEMO_POSITION_X: Integer := 20;
    MEMO_POSITION_Y: Integer := 340;
    MEMO_WIDTH: Integer := 400;
    MEMO_HEIGHT: Integer := 80;

    -- Buttons parameters
    BUTTONS_POSITION_X: Integer := 335;

    -- Initialize game window objects.
    My_Frame: Frame_Type := Frame (FRAME_WIDTH, FRAME_HEIGTH, FRAME_TITLE, Quit);
    My_Memo: Memo_Type := Memo (My_Frame, (MEMO_POSITION_X, MEMO_POSITION_Y), MEMO_WIDTH, MEMO_HEIGHT, Font ("Montserrat", 10));
    My_Button_Load: Button_Type := Button (My_Frame, (BUTTONS_POSITION_X, 50), 80, 40, "Load", Load);
    My_Button_Save: Button_Type := Button (My_Frame, (BUTTONS_POSITION_X, 100), 80, 40, "Save", Save);
    My_Button_Finish: Button_Type := Button (My_Frame, (BUTTONS_POSITION_X, 150), 80, 40, "Finish", Finish);
    My_Button_Show: Button_Type := Button (My_Frame, (BUTTONS_POSITION_X, 200), 80, 40, "Show", Show);

    -- Initialize sudoku grid blocks and base filename;
    My_Grid_Blocks: Grid_Block_Type;
    My_Matrix_Base_Filename: Unbounded_String := To_Unbounded_String("");
    
    -- Procedures defining commands of game buttons.

    -- Button LOAD
    Procedure Command_Load (grid_blocks: in out Grid_Block_Type; memo: in Memo_Type; base_filename: in out Unbounded_String) Is
        grid: Matrix_Type;
        file: Ada.Text_IO.File_Type;
        canceled: Boolean := False;
        base_filename_empty: Unbounded_String;
        Begin
            Begin
                JEWL.IO.Open(file, "Select sudoku matrix as .txt file");
            Exception
                when others =>
                    Ada.Text_IO.Put_Line ("JEWL FileOpeningError: No file selected.");
                    canceled := True;
            End;
            if not canceled then
                if fillGrid(readFile(file), grid, base_filename) then
                    Append_Line(memo, "Loaded file: '" & Ada.Text_IO.Name(file) & "'");
                    if (Ada.Strings.Fixed.Index(Ada.Text_IO.Name(file), "\" & To_String(base_filename), 1) > 0) then
                        Ada.Text_IO.Put_Line (Ada.Text_IO.Name(file) & " is base file");
                        base_filename_empty := To_Unbounded_String("");
                        setGrid(grid_blocks, grid, base_filename_empty);
                    else
                        Ada.Text_IO.Put_Line (Ada.Text_IO.Name(file) & " is user grid");
                        Ada.Text_IO.Put_Line (To_String(base_filename));
                        setGrid(grid_blocks, grid, base_filename);
                    end if;
                else
                    Append_Line(memo, "Load: Invalid Grid in choosen file.");
                end if;
                JEWL.IO.Close(file);
            else
                Append_Line(memo, "Load: No sudoku text file selected.");
            end if;
    End Command_Load;

    -- Button SAVE
    Procedure Command_Save (grid_blocks: in Grid_Block_Type; memo: in Memo_Type; base_filename: in out Unbounded_String) Is
        grid: Matrix_Type;
        filename: Unbounded_String;
        canceled: Boolean := False;
        Begin
            if getGrid(grid_blocks, grid, True) then
                showGrid(grid);
                Begin
                    filename := To_Unbounded_String(Get("Enter a filename:", "saved_matrix")); --unbounded : chaine de caractere de taille pas definie
                Exception
                    when others =>
                        Ada.Text_IO.Put_Line ("JEWL InputFileNameError: Canceled by user.");
                        canceled := True;
                End;
                if not canceled then
                    if To_String(filename)'Length > 0 then
                        if saveGrid(grid, To_String(filename), base_filename) then
                            Append_Line(memo, "Sudoku grid saved in '" & To_String(filename) & ".txt'"); -- to string permet transformer une chaine de caractere en taille definie necessaire pour ceratines fonctions
                        else
                            Append_Line(memo, "Save: Unable to save sudoku grid.");
                        end if;
                    else
                        Append_Line(memo, "Save: Invalid filename, try again.");
                    end if;
                end if;
            else
                Append_Line(memo, "Save: Unable to get valid sudoku grid.");
            end if;
    End Command_Save;

    -- Button FINISH
    Procedure Command_Finish (grid_blocks: in Grid_Block_Type; memo: in Memo_Type) Is
        grid: Matrix_Type;
        Begin
            if getGrid(grid_blocks, grid, False) then
                showGrid(grid);
                if isValidGrid(grid) then
                    Append_Line(memo, "Finish: Valid grid ! You have won ! :)");
                else
                    Append_Line(memo, "Finish: Invalid grid ! Try again..");
                end if;
            else
                Show_Error("Grid not filled or bad input.");
            end if;
    End Command_Finish;

    -- Button SHOW
    Procedure Command_Show (grid_blocks: in Grid_Block_Type) Is
        grid: Matrix_Type;
        Begin
            if getGrid(grid_blocks, grid, True) then
                showGrid(grid);
            else
                Show_Error("Unable to show grid.");
            end if;
    End Command_Show;

    -- Procedure to init game by loading a grid
    Procedure Init_Game (grid_blocks: in out Grid_Block_Type; memo: in Memo_Type) Is
        grid: Matrix_Type;
        base_filename: Unbounded_String;
        Begin
            Append_Line(memo, "Welcome to Sudoku 9x9 ! :)");
            createGrid(My_Frame, grid_blocks);
            base_filename := To_Unbounded_String("matrix_9.txt");
            if loadBaseMatrix(base_filename, grid) then
                base_filename := To_Unbounded_String("");
                setGrid(grid_blocks, grid, base_filename);
                Append_Line(memo, "Initial matrix_9.txt loaded.");
            end if;
    End Init_Game;

    -- Start of game
    Begin
        Init_Game(My_Grid_Blocks, My_Memo);
        if Valid(My_Frame) then
            loop
                case Next_Command is
                    when Quit =>
                        exit;
                    when Load =>
                        Command_Load(My_Grid_Blocks, My_Memo, My_Matrix_Base_Filename);
                    when Save =>
                        Command_Save(My_Grid_Blocks, My_Memo, My_Matrix_Base_Filename);
                    when Finish =>
                        Command_Finish(My_Grid_Blocks, My_Memo);
                    when Show =>
                        Command_Show(My_Grid_Blocks);
                    when others =>
                        null;
                end case;
            end loop;
        end if;

End sudoku_9;
