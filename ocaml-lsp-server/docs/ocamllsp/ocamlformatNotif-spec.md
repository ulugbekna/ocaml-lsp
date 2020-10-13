```typescript
interface OcamlformatUpdateSuccess extends NotificationMessage {
    method: "ocamlformat/update-success"
}

// FIXME use this instead of a string to represent the ocamlformat error
interface OcamlformatError {
    file: RelativePath, // (custom type)$$ relative to the project root
    range: Range,
    error: string
}

interface OcamlformatUpdateFailure extends NotificationMessage {
    method: "ocamlformat/update-failure",
    params: string
}
```

If ocamlformat ran successfully, `OcamlformatUpdateSuccess` is sent; otherwise,
`OcamlformatError` is sent.
