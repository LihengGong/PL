with Text_Io;
use Text_Io;

with ada.integer_text_io;
use ada.integer_text_io;

procedure doNothing is
LENGTH : constant Integer := 40;
MAXVAL : constant Integer := 300;
type Num is range -MAXVAL .. MAXVAL;
type Arr is array(1 .. LENGTH) of Num;

package Num_IO is new Text_Io.Integer_IO(Num);

  procedure printArr(sort_arr: Arr; lft, rght: in Integer) is
  idex : Integer := lft;
  begin
    for idex in lft .. rght loop
      Num_IO.Put(sort_arr(idex));
      Put(",");
    end loop;
    New_Line;
  end;

  procedure merge(sort_arr, spare_arr: in out Arr; lft, mddl, rght: in Integer) is
  idex : Integer := lft;
  fst : Integer := lft;
  scnd : Integer := mddl + 1;
  begin
    if lft > rght then
      return;
    end if;

    fst := lft;
    scnd := mddl + 1;
    idex := lft;
    while fst <= mddl and scnd <= rght loop
      if sort_arr(fst) < sort_arr(scnd) then
        spare_arr(idex) := sort_arr(fst);
        fst := fst + 1;
      else
        spare_arr(idex) := sort_arr(scnd);
        scnd := scnd + 1;
      end if;
      idex := idex + 1;
    end loop;

    while fst <= mddl loop
      spare_arr(idex) := sort_arr(fst);
      idex := idex + 1;
      fst := fst + 1;
    end loop;

    while scnd <= rght loop
      spare_arr(idex) := sort_arr(scnd);
      idex := idex + 1;
      scnd := scnd + 1;
    end loop;

    for idex in lft .. rght loop
      sort_arr(idex) := spare_arr(idex);
    end loop;
  end;

procedure mergeSort(sort_arr, spare_arr: in out Arr; lft, rght : in Integer) is
mddl : Integer := (lft + rght) / 2;
begin
  declare
    task T1;
    task T2;
    task body T1 is
    begin
      put("mergeSort1, lft1");
      put(lft); put(", rght1,"); put(rght);
      New_Line;
      if lft < rght then
        mergeSort(sort_arr, spare_arr, lft, mddl);
      end if;
    end T1;
    task body T2 is
    begin
      put("mergeSort2, lft2");
      put(lft); put(", rght2,"); put(rght);
      New_Line;
      if lft < rght then
        mergeSort(sort_arr, spare_arr, mddl + 1, rght);
      end if;
    end T2;
  begin
    null;
  end;
  if lft <= rght then
    merge(sort_arr, spare_arr, lft, mddl, rght);
  end if;
end;

  procedure testMerge is
    procedure getArr(sort_arr: out Arr; lft, rght: in Integer) is
      i : Integer := lft;
    begin
      for i in lft .. rght loop
        Num_IO.get(sort_arr(i));
      end loop;
    end getArr;

    procedure initArr(spare_arr: out Arr; lft, rght: in Integer) is
      i: Integer := lft;
    begin
      for i in lft .. rght loop
        spare_arr(i) := 0;
      end loop;
    end initArr;
  --sort_arr : Arr := 
  --(1, 3, 5, 7, 9, 2, 4, 6, 8, 10, 19, 18, 20, 13, 15, 17, 16, 12, 11, 14, 25, 21, 23, 29, 28, 22, 26, 27, 24, 30, 39, 38, 37, 36, 35, 34, 33, 32, 31, 40);
  --spare_arr : Arr :=
  --(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0);
  sort_arr : Arr;
  spare_arr : Arr;
  begin
    getArr(sort_arr, 1, LENGTH);

    mergeSort(sort_arr, spare_arr, 1, LENGTH);
    printArr(sort_arr, 1, LENGTH);
  end;

begin
  testMerge;
end;
