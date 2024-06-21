function slover_obj = configure_slover(input_file, roi_file, plane, slices, default)
    slover_obj = slover;
    slover_obj.cbar = 2;
    slover_obj.img(1).vol = spm_vol(input_file);
    slover_obj.img(1).type = 'structural';
    slover_obj.img(1).cmap = 'gray';
    if default.rangecolorscale == 1
        slover_obj.img(1).prop = 1;
    else
        slover_obj.img(1).range = default.rangecolorscale;
    end
    slover_obj.img(2).vol = spm_vol(roi_file);
    slover_obj.img(2).type = 'contour';
    slover_obj.img(2).cmap = 'nih.lut';
    slover_obj.img(2).range = [0.5 3.5];
    slover_obj.img(2).hold = 0;
    slover_obj.transform = plane;
    figure_position = strcmp(plane, 'axial') * [0, 0, default.final_width, default.final_height * 4/6] + ...
                      ~strcmp(plane, 'axial') * [0, 0, default.final_width, default.final_height * 1/6];
    slover_obj.figure = figure('Position', figure_position);
    slover_obj = fill_defaults(slover_obj);
    slover_obj.slices = slices;
    if strcmp(plane, 'coronal')
        slover_obj.slices = [-75 -45 35 53];
    elseif strcmp(plane, 'sagittal')
        slover_obj.slices(2) = 0;
    end
end