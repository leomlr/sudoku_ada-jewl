-- Add a John English's Window Library (JEWL).
with JEWL.IO;
with JEWL.Windows;
with Ada.Text_IO;
-- Add personal libraries.
with sudoku_9;
with sudoku_16;

Procedure sudoku Is
    use JEWL.IO;
    Type Command_Code is (Quit_Menu, Sudoku_9, Sudoku_16);
    package My_Game_Windows is new JEWL.Windows (Command_Code);
    use My_Game_Windows;

    -- Frame parameters
    FRAME_TITLE: String := "Sudoku";
    FRAME_WIDTH: Integer := 300;
    FRAME_HEIGTH: Integer := 300;

    -- Buttons parameters
    BUTTONS_POSITION_X: Integer := 90;

    My_Menu_Frame: Constant Frame_Type := Frame (FRAME_WIDTH, FRAME_HEIGTH, FRAME_TITLE, Quit_Menu);
    My_Button_Sudoku_9x9: Constant Button_Type := Button (My_Menu_Frame, (BUTTONS_POSITION_X, 50), 100, 50, "Sudoku_9", Sudoku_9);
    My_Button_Sudoku_16x16: Constant Button_Type := Button (My_Menu_Frame, (BUTTONS_POSITION_X, 120), 100, 50, "Sudoku_16", Sudoku_16);

    Procedure Command_Sudoku9 Is
        Begin
            sudoku_9;
        Exception
            when others =>
                Ada.Text_IO.Put_Line("Close of grid 9x9");
    End Command_Sudoku9;

    Procedure Command_Sudoku16 Is
        Begin
            sudoku_16;
        Exception
            when others =>
                Ada.Text_IO.Put_Line("Close of grid 16x16");
    End Command_Sudoku16;

    Begin
        if Valid(My_Menu_Frame) then
            loop
                case Next_Command is
                    when Quit_Menu =>
                        null;
                    when Sudoku_9 =>
                        Command_Sudoku9;
                    when Sudoku_16 =>
                        Command_Sudoku16;
                    when others =>
                        null;
                end case;
            end loop;
        end if;

End sudoku;