{
 /***************************************************************************
                                menus.pas
                                ---------

                   Initial Revision : Th Dec 10 CST 2020

 ***************************************************************************/

 *****************************************************************************
  This file is part of the Web Component Library (WCL)

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************
}
unit Menus;

{$I pas2js_widget.inc}

interface

uses
  Classes,
  SysUtils,
  Types,
  JS,
  Web,
  Graphics,
  Controls;

type

  { TMenuItem }

  TMenuItem = class(TCustomMenuItem)
  published
    property Caption;
    property OnClick;
  end;

implementation

end.

