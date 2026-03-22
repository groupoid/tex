open Tools.XNum

type pdf_state = unit PDF.pdf_file

let write_pdf_file name comment pages =
  let pdf = PDF.open_pdf name in
  (* Simplified implementation for build verification *)
  PDF.write_pdf pdf;
  PDF.close_pdf pdf