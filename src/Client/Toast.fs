module Toast

open Elmish.Toastr

let baseToast message =
    Toastr.message message
    |> Toastr.position TopRight

let error message =
    baseToast message
    |> Toastr.title "Error"
    |> Toastr.error

let success message =
    baseToast message
    |> Toastr.success

let warning message =
    baseToast message
    |> Toastr.title "Warning"
    |> Toastr.warning