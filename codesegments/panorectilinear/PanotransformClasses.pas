unit PanotransformClasses;


{ //
  todo:
  some of the equatorial maps are misaligned by +/- 1 by one pixel. Find and fix that :/
  in addition, the DLT creates an empty bottom line if 2*ch <> h (odd height/pixels)
// }


{ PanotransformClasses offer optimized transformations that connect
  a spherical panorama projection to other representations (transformed
  spherical representations or rectilinear projections). The analytical form
  of these transformations is inherently slow because inverse trigonometric functions
  are non-trivial to calculate. However, using several plane symmetries and common
  higher order approximation coefficients, the transformation from the unity sphere
  to the bitmap representation of a spherical panorama will be referred to as
  "source lookup".
  The neighbourhood of a point on the sphere near the equator transforms
  very much like said point so six projections of hemispheres onto the
  corresponding coordinate planes are generated. Each projection is represented
  by a grid of support vectors and first order derivatives that permit local
  bijective inversion. The chosen transform allows the lookup to take place by
  finding the associated hemisphere and dimension reduction. Coordinates are
  scaled and rounded to the nearest grid point. The source lookup transformations
  link rectilinear and curviliniear coordinates (the source representation axis
  are the polar angles) so the first derivative is a Jacobi matrix with non-vanishing
  non-diagonal elements that represent the shear of the rhomb to be drawn.

  For lookup from a spherical representation, a quarter of an image is used
  as lookup (destination lookup to get onto the unity sphere, lots of sin/cos calculations)
  and the other three quadrants are recovered by symmetry arguments. (90 degree rotations
  are achieved by coordinate permutations and sign changes)

  For lookup from a rectilinear viewport, the full viewport viewport can be represented
  by grid points. As the user zooms in or out, grid point density varies which makes
  a margin of precalculated grid points necessary and scale jumping mandatory. However, the
  first set of grid points to be calculated can neglect the zoom-out margin to trade
  on-demand recalculation for initalisation speed.
  This time, the lookup table is represented by grid points that reference to points on
  the unity sphere and are 3dim. The viewport can no longer be segmented into four quadrants
  because the fov is limited and the symmetry planes underlie orthogonal transformations.
  The transformations are applied to the grid points only, interpolation takes place afterwards.
  This even saves a good amount of 3x3 matrix * vector multiplications for the price of having
  to accumulate matrix transforms, occasional reorthonormalisation and refresh of the grid points
  that suffer from quantisation errors.

}



interface
  uses
    Windows, types, Graphics, math;



  type TLocalApproximationCoeff = record
    cx0,
    cy0 : single; // base coordinates can be easily calculated from array indices
  end;



  type TLocalJacobiMatrix = packed record
    j11, j12,
         j22: single; // j21 = 0 (dTheta/dx = 0)
  end;



  type TLocalPolarJMatrix = packed record
    j11, j12,
    j21, j22: single;
  end;



  type TSourceLookupEngine = class(TObject)
    constructor Create(w, h: integer; alpha_h , alpha_v: single; src: TBitmap; gridconst: integer = 16);
    destructor Destroy; override;
  private
    // source image variables
    //
    _buf : TBitmap;            // source image reference
    _offset, _linedelta: DWORD;      // 32bit image address calculation
    imgcenterx, imgcentery : single; // center of the bitmap
    _w, _h: single;

    // extended source image variables
    //
    _wrapthrsh : single;      // yneg table can contain wrapping boundary
    ppradx, pprady : single;  // pixels per radians (scaling coefficients)

    // supporting point variables
    //
    _center : longint;        // array index calculation constants for six tables (2 polar and 4 equatorial)
    _size : longint;          //
    xpos, xneg,
    ypos, yneg,
    zpos, zneg : Array of TLocalApproximationCoeff; // 0th order for each direction, avoid recalculations
    jmatrix    : Array of TLocalJacobiMatrix; // 1st order, equatorial mapping for x+-, y+-
    jpmatrix   : Array of TLocalPolarJMatrix; // 1st order, different mapping for z+-

    maptounity: single;       // map a table coordinate to its corresponding unity sphere coordinate
    unmapcoeff: single;       // reverse the above

    _zcenter : longint;       // array index calculation constants for the additional proximity tables
    _zsize : longint;
    hpos, hneg : Array of TLocalApproximationCoeff; // high-resolution z+- tables with pixel-exact mapping

    zmaptounity: single;      // mapping coefficients, different scaling
    zunmapcoeff: single;      //

    convangle, convz: single; // the convergence radius for the z tables (inside the circle, jpmatrix is no longer sufficient)
                              // corresponds to a threshold value of the abs(z) cooordinate
    zcalcrad2 : single;
  public
    tick: cardinal;           // performance testing only
    polarpinch: single;
    procedure ISLookup(var src_x, src_y: integer; var ux, uy, uz: single); // integer out, single in - lookup
  end;



  type TSupportVoxel = record
    ux, uy, uz: single;
  end;



  TSphericalLookupTransform = class(TObject)
    constructor Create(targetsize: TPoint; alpha_h : single);
    destructor Destroy; override;
  private
    vptab : Array of TSupportVoxel; // viewport table (first quadrant)
    cx, cy, w, h: integer; // viewport center
    lind: integer; // line delta in units of elements
    pprad, radiansperpx : single;
  public
    procedure lookupUR(var x, y: integer; var ux, uy, uz: single); // upper right quadrant
    procedure lookupUL(var x, y: integer; var ux, uy, uz: single); // upper left quadrant
    procedure lookupLL(var x, y: integer; var ux, uy, uz: single); // lower left quadrant
    procedure lookupLR(var x, y: integer; var ux, uy, uz: single); // lower right quadrant
    procedure Lookup(var x, y: integer; var ux, uy, uz: single);
  end;



  {type TDestinationLookupTransform = class(TObject)
  end;}


  type TViewportParameters = record
    _focallen: single;
    theta, phi, psi: single;
    viewportsize: TPoint;
    coarsepreview: boolean;
    aspect: string[8];
  end;



implementation

const
  INVALID_LOCATION = high(integer);



/////////////////////////////////////////////////////////////////////////////
//  TSourceLookupEngine implementation
/////////////////////////////////////////////////////////////////////////////


constructor TSourceLookupEngine.Create(
    w, h : integer; alpha_h, alpha_v : single;   // w, h    : source bitmap width and height
    src: TBitmap; gridconst: integer);           // alpha_h/v : angle corresponding to the width  w  /height h (in degrees)
var
  pixelsperdeg : single; // universal angular pixel density (for isotropic images)

  entry   : ^TLocalApproximationCoeff; // array element pointers
  jentry  : ^TLocalJacobiMatrix;
  jpentry : ^TLocalPolarJMatrix;

  x, y : integer; // local loop variables

  ux, uy, uz : single; // unity sphere coordinates (ux, uy, uz)
  phi, theta : single;
  rho, rho2  : single; // distance from the z-axis and its square value

  r2, y0: single;     // common values for x+-, y+- tables
begin

  inherited Create;

  phi := 0; theta := 0; y0 := 0; // silence the compiler
  polarpinch :=0;

  // initial fixup to avoid division errors
  //
  if gridconst <  1 then gridconst :=  1;
  if alpha_h   <= 0 then alpha_h   :=  1;
  if alpha_v   <= 0 then alpha_v   :=  1;
  if gridconst <  2 then gridconst :=  2;
  if gridconst > 48 then gridconst := 48;

  // address calculations
  //
  _buf := src;
  _offset := Cardinal(Pointer(_buf.ScanLine[0]));
  _linedelta := _offset - Cardinal(Pointer(_buf.Scanline[1]));
  _w := src.Width;
  _h := src.Height;
  _wrapthrsh := (src.Width-1) * 360 / alpha_h;

  // the maximum angular source pixel density is quite straight forward
  //
  ppradx := (w-1)/(alpha_h*pi/180);
  pprady := (h-1)/(alpha_v*pi/180);
  pixelsperdeg  := min( w/alpha_h , h/alpha_v );

  // example: a 360° x 180° pano has 8000 x 4000 pixels
  // so there are about 22 pixels / deg
  // each table has to represent the surface points in the half space
  // where z > 0.5 (-z, +-x, +-y alike)  and these points are within a circle
  // with r = 0.87. there are 4000 pixels on a 180° arc that is pi*r = pi (r=1)
  // long and this arc is projected onto a line which is 2r = 2 long, hence the 2/pi
  // factor. the height z = 0.5 corresponds to a polar angle of 60° so the relevant
  // points are within a circle of sqrt(3)/2 ~ 0.866. the table size can hence
  // be reduced by this factor. The grid coverage drops to 2*gridsize at the edges.
  //
  _size := round(((pixelsperdeg * 180) * 2/pi) * 0.95 / gridconst+0.5)+2;  // 0.9 <-> 1/2 sqrt(3) + safety margin
  //_size := size;
  _center := _size div 2;
  maptounity :=  2 / (((pixelsperdeg * 180) * 2/pi) / gridconst);
  unmapcoeff := 1 / maptounity;


  // equatorial maps are a very good approximation for grid constants under 32.
  // linear approximation fails around the poles, so immediate pixel maps are added.
  // the convergence radius is the threshold value at which switching to immediate mapping
  // becomes mandatory. Set the cut to 4 gridconst units in diameter.
  //
  convangle := 2 * gridconst / ppradx;
  convz := cos(convangle);

  // memory allocation
  setlength(xpos, _size * _size * sizeof(TLocalApproximationCoeff));
  setlength(xneg, length(xpos) );
  setlength(ypos, length(xpos) );
  setlength(yneg, length(xpos) );
  setlength(zpos, length(xpos) );
  setlength(zneg, length(xpos) );
  setlength(jmatrix , _size * _size * sizeof(TLocalJacobiMatrix));
  setlength(jpmatrix, _size * _size * sizeof(TLocalPolarJMatrix));

  // precalculate further constants
  imgcenterx := (w-1) / 2;
  imgcentery := (h-1) / 2;

  tick := GetTickCount; // start speed evaluation

// calculations for all tables, process in a single loop using similarities and symmetries
//
  for y := 0 to _size -1 do
  begin
    for x := 0 to _size -1 do  // fill only a circular patch like center -..+ sqrt((center)^2-(y-center)^2), save 22%
    begin

/////////////////////////////////////////////////////////////////////////////

// general purpose calculations
//
      ux    := (x-_center) * maptounity; // x any y coordinates on the unity sphere, calculate z later if needed
      uy    := (y-_center) * maptounity;
      rho2  := ux*ux + uy*uy; // distance from the z axis as the cylindical coordinate rho2
      rho   := sqrt(rho2);

// positive z lookup table
//
      entry := @zpos[x + y*_size]; // select current data set

      if rho2 > (0.940*0.940) then
      begin
          entry^.cx0 := INVALID_LOCATION; // location out of bounds
      end
      else
      begin
          uz := sqrt(1-rho2); // calculate z coordinate
          theta := arccos(uz); // calculate polar angle

          if ux <> 0 then  // calculate azimuthal angle
          begin
             phi := arctan(uy/ux)-pi/2;
             if ux < 0 then phi := phi+ pi;
          end
          else
          begin
             if uy > 0 then phi := 0 else phi := -pi;
          end;

          // calculate final interpolation coefficients
          entry^.cx0 := imgcenterx + ppradx * phi;
          entry^.cy0 := imgcentery + pprady * (theta-pi/2);

        //
        // 1st order jacobi matrix for polar lookup tables
        // I know this doesn't look good but here is the place to do the calculations

          jpentry := @jpmatrix[y + x*_size];

          if (rho2 > 0) then
          begin
            jpentry^.j11 :=  gridconst * (  uy / rho2 );               // dphi / dx
            jpentry^.j12 :=  gridconst * ( -ux / rho2 );               // dphi / dy
            jpentry^.j21 :=  gridconst * (  ux / (sqrt(1-rho2)*rho) ); // dTheta / dx
            jpentry^.j22 :=  gridconst * (  uy / (sqrt(1-rho2)*rho) ); // dTheta / dy
          end
          else
          begin
            jpentry^.j11 := 0;
            jpentry^.j12 := 0;
            jpentry^.j21 := 0;
            jpentry^.j22 := 0;
          end;
        // resume z table calculation
        //

      end;


// negative z lookup table : use mirror symmetry to derive it from the positive table
//
      entry := @zneg[x + y*_size]; // select current data set

      if rho2 > (0.940*0.940) then
      begin
          entry^.cx0 := INVALID_LOCATION; // location out of bounds
      end
      else
      begin
          theta := pi-theta;
          phi := phi + pi;
          if phi > pi then phi := phi - 2*pi;

          // calculate final interpolation coefficients
          entry^.cx0 := imgcenterx + ppradx * phi;
          entry^.cy0 := imgcentery + pprady * (theta-pi/2);
      end;

//----------------------------------------------------------------------------

// general purpose calculations (changed declarations)
//
      uz    := -(x-_center) * maptounity; // redefine; x any y coordinates on the
                                          // unity sphere, calculate z later if needed
      // uy    :=  (y-_center) * maptounity;  // already calculated that above;
      r2    := uz*uz + uy*uy; // distance from the x axis as the cylindical coordinate
                              // rho2, just for the coverage criterion
                              // applies to y axis as well when symmetry is used

// positive x lookup points
//
      entry := @xpos[y + x*_size]; // select current data set

      if r2 > (0.940*0.940) then
      begin
          entry^.cx0 := INVALID_LOCATION; // location out of bounds
      end
      else
      begin
          ux := sqrt(1-r2); // calculate z coordinate, remember the redeclaration

        //
        // 1st order coefficients calculation for all subsequent table calculations
              // former implementation :
              //   entry^.cx1 := gridconst * (1/ux); // dphi/duy
              //   entry^.m12 := gridconst * (-uy*uz / (ux*(ux*ux + uy*uy))); // dphi/duz
              //   entry^.m21 := 0; // dTheta/duy = 0!
              //   entry^.cy1 := gridconst * (1/ sqrt(1-uz*uz)) ; // dTheta/duz
              //
        // I know this doesn't look good but here is the place to
        // do the calculations, inside a r2<= 0.9*0.9 block
          jentry := @jmatrix[y + x*_size];
          jentry^.j11 :=   gridconst / ux;
          jentry^.j12 := - gridconst * (uy * uz / (ux*(ux*ux + uy*uy)));
          jentry^.j22 :=   gridconst / sqrt(ux*ux + uy*uy);
        // resume x lookup calculations ...
        //

          theta := arccos(uz); // calculate polar angle
          if ux <> 0 then  // calculate azimuthal angle
          begin
             phi := arctan(uy/ux)-pi/2;
             if ux < 0 then phi := phi+ pi;
          end
          else
          begin
             if uy > 0 then phi := 0 else phi := -pi;
          end;

          // calculate final interpolation coefficients, 0th order first
          entry^.cx0 := imgcenterx + ppradx * phi;
          entry^.cy0 := imgcentery + pprady * (theta-pi/2);
          y0 := entry^.cy0; // store for subsequent assignments
      end;


// negative x lookup table (symmetry again)
//
      entry := @xneg[y + x*_size]; // select current data set

      if r2 > (0.940*0.940) then
      begin
          entry^.cx0 := INVALID_LOCATION; // location out of bounds
      end
      else
      begin
          phi := pi + phi;

          entry^.cx0 := imgcenterx + ppradx * phi;
          entry^.cy0 := y0;
      end;

//----------------------------------------------------------------------------

// positive y lookup points
//
      entry := @ypos[y + x*_size]; // select current data set

      if r2 > (0.940*0.940) then
      begin
          entry^.cx0 := INVALID_LOCATION; // location out of bounds
      end
      else
      begin
          phi := phi - pi/2;

          entry^.cx0 := imgcenterx + ppradx * phi;
          entry^.cy0 := y0;
      end;

// negative y lookup table
//
      entry := @yneg[y + x*_size]; // select current data set

      if r2 > (0.940*0.940) then
      begin
          entry^.cx0 := INVALID_LOCATION; // location out of bounds
      end
      else
      begin
          phi := pi + phi;
          if phi >= pi then phi := phi - 2*pi;

          entry^.cx0 := imgcenterx + ppradx * phi;
          entry^.cy0 := y0;
      end;


    end; // for x
  end; // for y



//----------------------------------------------------------------------------
// ah well, the additional z proximity maps ...
//----------------------------------------------------------------------------
  _zsize := round( pixelsperdeg * ( 2 * convangle * 180/pi) + 2 );
  _zcenter := _zsize div 2;
  zmaptounity :=  pi / (pixelsperdeg * 180);
  zunmapcoeff :=   1 / zmaptounity;

  setlength(hpos, _zsize * _zsize * sizeof(TLocalApproximationCoeff));
  setlength(hneg, _zsize * _zsize * sizeof(TLocalApproximationCoeff));

// pixel-exact maps need to be calculated here, much like the above, but with totally different values

  zcalcrad2 := sqr(_zcenter * zmaptounity);

  for y := 0 to _zsize -1 do
  begin
    for x := 0 to _zsize -1 do
    begin
      ux    := (x-_zcenter) * zmaptounity;
      uy    := (y-_zcenter) * zmaptounity;
      rho2  := ux*ux + uy*uy;

    // positive proximity map (north pole)
      entry := @hpos[x + y*_zsize];

      if rho2 > (zcalcrad2) then
      begin
          entry^.cx0 := INVALID_LOCATION; // location out of bounds
      end
      else
      begin
          uz  := sqrt(1-rho2); // calculate z coordinate
          theta := arccos(uz); // calculate polar angle

          if ux <> 0 then      // calculate azimuthal angle
          begin
             phi := arctan(uy/ux)-pi/2;
             if ux < 0 then phi := phi+ pi;
          end
          else
          begin
             if uy > 0 then phi := 0 else phi := -pi;
          end;

          entry^.cx0 := imgcenterx + ppradx * phi;
          entry^.cy0 := imgcentery + pprady * (theta-pi/2);
      end;

      // negative proximity map (south pole)
      entry := @hneg[x + y*_zsize]; // select current data set

      if rho2 > (zcalcrad2) then
      begin
          entry^.cx0 := INVALID_LOCATION; // location out of bounds
      end
      else
      begin
          theta := pi-theta;
          phi := phi + pi;
          if phi > pi then phi := phi - 2*pi;

          entry^.cx0 := imgcenterx + ppradx * phi;
          entry^.cy0 := imgcentery + pprady * (theta-pi/2);
      end;

    end;
  end;
//----------------------------------------------------------------------------
// finish initalisation
  tick := gettickcount - tick; // stop time measurement. Tick now contains the recent time duration
end;



destructor TSourceLookupEngine.Destroy;
begin
  setlength(xpos, 0 );
  setlength(xneg, 0 );
  setlength(ypos, 0 );
  setlength(yneg, 0 );
  setlength(zpos, 0 );
  setlength(zneg, 0 );
  setlength(jmatrix , 0 );
  setlength(jpmatrix, 0 );
  setlength(hpos, 0 );
  setlength(hneg, 0 );

  inherited Destroy;
end;



procedure TSourceLookupEngine.ISLookup(var src_x, src_y: integer; var ux, uy, uz: single);
var
  basex, basey: longint;          // base vectors (integer quantisation
  bx, by, deltax, deltay: single; // fraction (-0.5 .. 0.5) and single-value position
  idx: longint;                   // 0 order index: idx, also applies to 1st order local jacobi matrix (same resolution and size)
  tmp: single;                    // for further testing, src_x needs a backup (before rounding)
begin

// select the appropriate hemisphere, use the assigned lookup table and jmatrix table
// to obtain approximation coefficients for x+-, y+- cases
//
  if abs(ux) >= 0.5 then
  begin
    if ux > 0 then
    begin
      bx := ( uy*unmapcoeff+self._center);
      by := (-uz*unmapcoeff+self._center);
      basex := round(bx);
      basey := round(by);
      deltax := bx - basex;
      deltay := by - basey;
      idx := basex+_size*basey;
      src_x := round(xpos[idx].cx0 + jmatrix[idx].j11 * deltax + jmatrix[idx].j12 * deltay);
      src_y := round(xpos[idx].cy0{+ 0}                        + jmatrix[idx].j22 * deltay-polarpinch);
      exit;
    end
    else
    begin
      bx := (-uy*unmapcoeff+self._center);
      by := (-uz*unmapcoeff+self._center);
      basex := round(bx);
      basey := round(by);
      deltax := bx - basex;
      deltay := by - basey;
      idx := basex+_size*basey;
      src_x := round(xneg[idx].cx0 + jmatrix[idx].j11 * deltax + jmatrix[idx].j12 * deltay);
      src_y := round(xneg[idx].cy0{+ 0}                        + jmatrix[idx].j22 * deltay-polarpinch);
      exit;
    end;
  end;


  if abs(uy) >= 0.5 then
  begin
    if uy > 0 then
    begin
      bx := (-ux*unmapcoeff+self._center);
      by := (-uz*unmapcoeff+self._center);
      basex := round(bx);
      basey := round(by);
      deltax := bx - basex;
      deltay := by - basey;
      idx := basex+_size*basey;
      src_x := round(ypos[idx].cx0 + jmatrix[idx].j11 * deltax + jmatrix[idx].j12 * deltay);
      src_y := round(ypos[idx].cy0{+ 0}                        + jmatrix[idx].j22 * deltay-polarpinch);
      exit;
    end
    else
    begin
      bx := ( ux*unmapcoeff+self._center);
      by := (-uz*unmapcoeff+self._center);
      basex := round(bx);
      basey := round(by);
      deltax := bx - basex;
      deltay := by - basey;
      idx := basex+_size*basey;
      tmp   :=      (yneg[idx].cx0 + jmatrix[idx].j11 * deltax + jmatrix[idx].j12 * deltay);
      src_y := round(yneg[idx].cy0{+ 0}                        + jmatrix[idx].j22 * deltay-polarpinch);
      
      if tmp < _wrapthrsh then
        begin
          if tmp >= 0 then
          begin
            src_x := round(tmp);
            exit;
          end
          else
          begin
            src_x := round(tmp+_wrapthrsh);
            exit;
          end;
        end
      else
        begin
          // azimuthal wrap required because pixels from the left margin are referenced to the
          // lookup table points. This could be fixed by choosing the right rounding method, but
          // floor, ceil and trunc are slower than round. maybe round(singlevar - 0.4999999) will do the trick?
          //
          src_x := round(tmp-_wrapthrsh);
          exit;
        end;

    end;
  end;

  
// z polar caps need different treatment
//
  if (abs(uz) >= 0.5) and (abs(uz) < convz) then
  begin
    if uz > 0 then
    begin
      bx := (ux*unmapcoeff+self._center);
      by := (uy*unmapcoeff+self._center);
      basex := round(bx);
      basey := round(by);
      deltax := bx - basex;
      deltay := by - basey;
      idx := basex+_size*basey;
      tmp   :=      (zpos[idx].cx0 + jpmatrix[idx].j12 * deltax + jpmatrix[idx].j11 * deltay);
      src_y := round(zpos[idx].cy0 + jpmatrix[idx].j22 * deltax + jpmatrix[idx].j21 * deltay-polarpinch);
      if tmp < _wrapthrsh then
        begin
          if tmp >= 0 then
          begin
            src_x := round(tmp);
            exit;
          end
          else
          begin
            src_x := round(tmp+_wrapthrsh);
            exit;
          end;
        end
      else
        begin
          src_x := round(tmp-_wrapthrsh);
          exit;
        end;
    end
    else
    begin
      bx := (-ux*unmapcoeff+self._center);
      by := (-uy*unmapcoeff+self._center);
      basex := round(bx);
      basey := round(by);
      deltax := bx - basex;
      deltay := by - basey;
      idx := basex+_size*basey;
      tmp :=        (zneg[idx].cx0 + jpmatrix[idx].j12 * deltax + jpmatrix[idx].j11 * deltay);
      src_y := round(zneg[idx].cy0 - jpmatrix[idx].j22 * deltax - jpmatrix[idx].j21 * deltay-polarpinch);
      if tmp < _wrapthrsh then
        begin
          if tmp >= 0 then
          begin
            src_x := round(tmp);
            exit;
          end
          else
          begin
            src_x := round(tmp+_wrapthrsh);
            exit;
          end;
        end
      else
        begin
          src_x := round(tmp-_wrapthrsh);
          exit;
        end;
    end;
  end
  else  // proximity to polar singularity, switch to direct mapping mode
  begin
    if uz > 0 then
    begin
      basex := round(ux * zunmapcoeff + _zcenter);
      basey := round(uy * zunmapcoeff + _zcenter);
      idx := basex+_zsize*basey;
      src_x := round(hpos[idx].cx0);
      src_y := round(hpos[idx].cy0-polarpinch);
      exit;
    end
    else
    begin
      basex := round(-ux * zunmapcoeff + _zcenter);
      basey := round(-uy * zunmapcoeff + _zcenter);
      idx := basex+_zsize*basey;
      src_x := round(hneg[idx].cx0);
      src_y := round(hneg[idx].cy0-polarpinch);
      exit;
    end;
  end;

end;



///////////////////////////////////////////////////////////////////////////////
//    TSphericalLookupTransform
///////////////////////////////////////////////////////////////////////////////



constructor TSphericalLookupTransform.Create(targetsize: TPoint; alpha_h : single);
var
  nx, ny : integer;
  x, y : integer;
  entry : ^TSupportVoxel;
  phi, theta : single;
begin
  inherited Create;

  w := targetsize.X;
  h := targetsize.Y;

  nx := (w + 1) div 2+1;
  ny := (h + 1) div 2+1;

  cx := (w-1) div 2;
  cy := (h-1) div 2;

  setlength(vptab, nx * ny * sizeof(TSupportVoxel) );
  lind := nx;

  radiansperpx := pi/180 * alpha_h / targetsize.X;
  pprad := 1 / radiansperpx;

  for y := 0 to ny-1 do
  begin
    for x := 0 to nx-1 do
    begin
      entry := @vptab[x + lind * y];

      theta := pi/2 + radiansperpx * y;
      phi   :=        radiansperpx * x;

      asm
          push EAX
          mov EAX, DWORD PTR entry
          fld theta
          fsincos
          fstp [EAX+$08]
          fld phi
          fsincos
          fmul st(0), st(2)
          fstp [EAX+$00]
          fmul st(0), st(1)
          fstp [EAX+$04]
          fstp st(0)
          pop EAX
      end; // asm

    end; // for x
  end; // for y
end;


destructor TSphericalLookupTransform.Destroy;
begin
  setlength(vptab, 0);

  inherited destroy;
end;



procedure TSphericalLookupTransform.lookupUR(
        var x, y: integer; var ux, uy, uz: single);
var
  entry : ^TSupportVoxel;
begin
  entry := @vptab[x + lind * y];
  ux := entry^.ux;
  uy := entry^.uy;
  uz := entry^.uz;
end;



procedure TSphericalLookupTransform.lookupUL(
        var x, y: integer; var ux, uy, uz: single);
var
  entry : ^TSupportVoxel;
begin
  entry := @vptab[x + lind * y];
  ux := entry^.ux;
  uy := -entry^.uy;
  uz := entry^.uz;
end;



procedure TSphericalLookupTransform.lookupLL(
        var x, y: integer; var ux, uy, uz: single);
var
  entry : ^TSupportVoxel;
begin
  entry := @vptab[x + lind * y];
  ux := entry^.ux;
  uy := -entry^.uy;
  uz := -entry^.uz;
end;




procedure TSphericalLookupTransform.lookupLR(
        var x, y: integer; var ux, uy, uz: single);
var
  entry : ^TSupportVoxel;
begin
  entry := @vptab[x + lind * y];
  ux := entry^.ux;
  uy := entry^.uy;
  uz := -entry^.uz;
end;




procedure TSphericalLookupTransform.Lookup(
        var x, y: integer; var ux, uy, uz: single);
var
  _x, _y : integer;
  entry : ^TSupportVoxel;
begin
  _x := x - cx;
  _y := y - cy;

  if _x >= 0 then
  begin
    if _y >= 0 then
    begin
      //LookupUR(_x, _y, ux, uy, uz);
      entry := @vptab[_x + lind * _y];
      ux := entry^.ux;
      uy := entry^.uy;
      uz := entry^.uz;
    end
    else
    begin
      _y := -_y+1;
      //LookupLR(_x, _y, ux, uy, uz);
      entry := @vptab[_x + lind * _y];
      ux :=  entry^.ux;
      uy :=  entry^.uy;
      uz := -entry^.uz;
    end;
  end
  else
  begin
    _x := -_x+1;
    if _y >= 0 then
    begin
      //LookupUL(_x, _y, ux, uy, uz);
      entry := @vptab[_x + lind * _y];
      ux :=  entry^.ux;
      uy := -entry^.uy;
      uz :=  entry^.uz;
    end
    else
    begin
      _y := -_y+1;
      //LookupLL(_x, _y, ux, uy, uz);
      entry := @vptab[_x + lind * _y];
      ux :=  entry^.ux;
      uy := -entry^.uy;
      uz := -entry^.uz;
    end;
  end;
end;



end.
