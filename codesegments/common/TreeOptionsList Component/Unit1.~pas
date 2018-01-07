unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ImgList, Math, ExtCtrls, ComCtrls, TreeOptionListCmp ;

type
  TForm1 = class(TForm)
    ImageList1: TImageList;
    TypeImages: TImageList;
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    list: TTreeOptionList;
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.FormCreate(Sender: TObject);
var
  node, subnode: TTreeNode;
begin
  inherited;

  list := TTreeOptionList.Create(self);
  list.Parent := self;
  list.PropertyCount := 4;
  list.PropertyIconList := ImageList1;
  list.Tree.Images := TypeImages;
  list.Align := alclient;

  self.Constraints.MinWidth := 250;

  node := List.Tree.Items.Add(nil,'Node');
  node.StateIndex := 0;
  node.ImageIndex := 0;
  node.SelectedIndex := node.ImageIndex;

  subnode := List.Tree.Items.AddChild(node,'Child Node');
  subnode.StateIndex := 0;
  subnode.ImageIndex := 1;
  subnode.SelectedIndex := subnode.ImageIndex;

  subnode := List.Tree.Items.AddChild(node,'Child Node');
  subnode.StateIndex := 0;
  subnode.ImageIndex := 1;
  subnode.SelectedIndex := subnode.ImageIndex;

  subnode := List.Tree.Items.AddChild(node,'Child Node');
  subnode.StateIndex := 0;
  subnode.ImageIndex := 2;
  subnode.SelectedIndex := subnode.ImageIndex;

  node := List.Tree.Items.Add(nil,'Node');
  node.StateIndex := 0;
  node.ImageIndex := 0;
  node.SelectedIndex := node.ImageIndex;

  subnode := List.Tree.Items.AddChild(node,'Child Node');
  subnode.StateIndex := 0;
  subnode.ImageIndex := 3;
  subnode.SelectedIndex := subnode.ImageIndex;
  subnode := List.Tree.Items.AddChild(node,'Child Node');
  subnode.StateIndex := 0;
  subnode.ImageIndex := 3;
  subnode.SelectedIndex := subnode.ImageIndex;
  subnode := List.Tree.Items.AddChild(node,'Child Node');
  subnode.StateIndex := 0;
  subnode.ImageIndex := 3;
  subnode.SelectedIndex := subnode.ImageIndex;

end;

end.
