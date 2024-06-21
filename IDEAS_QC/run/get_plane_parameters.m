function [slice_num, start_slice, stop_slice, panels] = get_plane_parameters(plane)
    switch plane
        case 'coronal'
            slice_num = 4; start_slice = -65; stop_slice = -10; panels = [4; 1];
        case 'axial'
            slice_num = 16; start_slice = -40; stop_slice = start_slice + 80; panels = [4; 4];
        case 'sagittal'
            slice_num = 4; start_slice = -28; stop_slice = start_slice + 56; panels = [4; 1];
    end
end