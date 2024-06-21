function create_multislice(input_file, roi_files, output_dir, default, overwrite)
    % Function to create multislices for image quality control
    %
    % Parameters 
    % --------------------------------------------------------
    % input_file: char or str
    %   Full name of .nii file to review.
    %
    % roi_files: char or str
    %   Full name of .nii regions of interest.
    %
    % output_dir: char or str
    %   Path to desired output directory
    %
    % overwrite: logical, optional
    %   If true, overwrite existing files
    %
    % default: array
    %   Default parameters

     arguments
        input_file {mustBeText}
        roi_files {mustBeText} 
        output_dir {mustBeFolder} 
        default
        overwrite logical = false;
     end

%Gets the scan id
[~, scan_id, ~] = fileparts(input_file);

% Iterates over planes and image versions
for incl_con = 0:1
    output_filename = fullfile(output_dir, sprintf('%s_%s.%s', scan_id, num2str(incl_con), default.img_extension));
    if exist(output_filename, 'file') && ~overwrite
        fprintf('\nFile exist, will skip.\n-------------------\n');
    else
        for i = 1:numel(default.planes)
            plane = char(default.planes(i));
    
                for j = 1:size(roi_files, 1)
                    filename = fullfile(output_dir, sprintf('%s_inclcon%d_%s.%s', scan_id, incl_con, plane, default.img_extension));
        
                    % Define slices and panels based on plane
                    [slice_num, start_slice, stop_slice, panels] = get_plane_parameters(plane);
        
                    slices = round(linspace(start_slice, stop_slice, slice_num));
        
                    % Create and configure slover object
                    slover_obj = configure_slover(input_file, char(roi_files(j)), plane, slices, default);
        
                    % Generate figure with the desired parameter
                    new_paint(slover_obj, incl_con, j-1, i, panels, scan_id);
        
                    if j > 1
                        % Save the image without white borders
                        pause(1);
                        export_fig(gcf, filename, '-r300', '-silent');
                        close;
        
                        % Resize image to the desired dimensions
                        I1 = imread(filename);
                        resized_height = strcmp(plane, 'axial') * default.final_height * 4/6 + ~strcmp(plane, 'axial') * default.final_height * 1/6;
                        I1 = imresize(I1, [resized_height, default.final_width]);
                        imwrite(I1, filename, 'Resolution', 300);
                        close;
                    end
                end
        end
        
        % Assemble the previously created images for each view ([4,4] for axial, [1,4] for sagittal, and [1,4] for coronal) into a single figure
        %Loads all images in 3 planes
        axial_name = sprintf('%s/%s_inclcon%d_%s.%s', output_dir, scan_id, incl_con, char(default.planes(1)), char(default.img_extension));
        sagittal_name = sprintf('%s/%s_inclcon%d_%s.%s', output_dir, scan_id, incl_con, char(default.planes(2)), char(default.img_extension));
        coronal_name = sprintf('%s/%s_inclcon%d_%s.%s', output_dir, scan_id, incl_con, char(default.planes(3)), char(default.img_extension));
        I_axial = imread(axial_name);
        I_sagittal =imread(sagittal_name);
        I_coronal = imread(coronal_name);
    
        %Stacks them together
        I_final = [I_axial; I_sagittal; I_coronal];
        
        %Crops the image to end with a 6x4 grid
        imwrite(I_final, output_filename, 'Resolution', 300);
        close all;
        delete(axial_name); delete(sagittal_name); delete(coronal_name);
    end
end
delete('i2s*.mat'); delete('yxratio_*.mat');